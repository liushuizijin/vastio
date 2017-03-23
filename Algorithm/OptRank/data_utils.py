#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
定义数据导入、数据转换、数据输出、关键计算相关的函数

"""

import json
import numpy as np
import pandas as pd


def read_config(config_path):
    with open(config_path + '/config.json', 'r') as f:
        config = json.load(f)
    pd_id = str(config['pd_id'])
    data_in_path = config['data_in_path']
    model_path = config['model_path']
    start_date = str(config['start_date'])
    end_date = str(config['end_date'])
    date_range = config['date_range']
    top_n = config['top_n']
    data_out_path = config['data_out_path']
    return pd_id, data_in_path, model_path, start_date, end_date, date_range, top_n, data_out_path


def data_input(data_path):
    """
    :param data_path: str,数据路径文件名加后缀类型(csv格式)
    :return: DataFrame,可能存在日期间断的案件数据
    """
    crime_data = pd.read_csv(data_path, header=None)
    # class_count = int((len(crime_data.columns) - 2) / 2)
    class_count = int((crime_data.shape[1] - 2) / 2)

    if class_count == 1:
        crime_data.columns = ["date_str", "grid_num", "noclass_layer0", "noclass_layer0_1"]
    if class_count == 2:
        crime_data.columns = ["date_str", "grid_num", "class1_layer0", "class1_layer0_1",
                              "class2_layer0", "class2_layer0_1"]
    if class_count == 3:
        crime_data.columns = ["date_str", "grid_num", "class1_layer0", "class1_layer0_1",
                              "class2_layer0", "class2_layer0_1", "class3_layer0", "class3_layer0_1"]
    crime_data[crime_data.ix[:, 2:len(crime_data.columns)] >= 1] = 1
    return crime_data, class_count


def date_grid(start_date, end_date, date_range, crime_data):
    """
    :param start_date: str,训练数据的起始日期,input进来的数据的起始日期需要是start_date-date_range或更早的日期,start_date是闭区间
    :param end_date: str,训练数据的结束日期,开区间
    :param date_range: Number,推移天数(相当于热点期天数)
    :param crime_data: DataFrame,可能存在日期间断的案件数据
    :return: 3个array,连续补全的日期(不同起始时间)序列数据框,格子集合数据框(crime_data数据集中存在的所有格子)
    """
    date_array = pd.date_range(start_date, end_date, freq='D')  # 全闭区间,注意start_date和end_date是左闭右开的
    date_array = date_array.strftime('%Y%m%d').astype('int')[:-1]

    date_range_array = pd.date_range(pd.date_range(start_date, periods=1, freq='D').shift(-date_range).strftime('%Y%m%d')[0],
                                     end_date, freq='D').strftime('%Y%m%d').astype('int')[:-1]

    grid_array = crime_data.grid_num.unique()
    return date_array, date_range_array, grid_array


def data_continuity(date, grid_array, crime_data):
    """
    :param date: array,date_df or date_range_df,连续日期数据框
    :param grid_array: array,格子集合数据框
    :param crime_data: DataFrame,可能存在日期间断的案件数据
    :return: DataFrame,连续日期补全的案件数据
    """
    date_df = pd.DataFrame({'date_str': date.repeat(grid_array.shape[0])})
    grid_df = pd.DataFrame({'grid_num': grid_array.tolist() * date.shape[0]})
    crime_data_continuity = pd.concat([date_df, grid_df], axis=1)
    crime_data_continuity = pd.merge(crime_data_continuity, crime_data, how='left', on=['date_str', 'grid_num'])
    crime_data_continuity = crime_data_continuity.fillna(0)
    return crime_data_continuity


def data_label(crime_data_continuity_norange):
    """
    :param crime_data_continuity_norange: DataFrame,3列，'date_str','grid_num','case_nums'(本格子是否有案件 或 外层格子是否有案件)
    :return: DataFrame,返回对应训练数据行记录顺序的案发格子列表的列表
    """
    crime_data_continuity_norange.columns = ['date_str', 'grid_num', 'value']
    date_array = pd.DataFrame({'date_str': crime_data_continuity_norange.date_str.unique()})
    label_data = crime_data_continuity_norange.ix[crime_data_continuity_norange.value == 1, :2]
    label_data = label_data.groupby('date_str', as_index=False).aggregate(lambda x: list(x))
    label_data = pd.merge(date_array, label_data, how='left', on='date_str')
    label_data = label_data.sort_values(by='date_str', axis=0)
    # 某天无案件时为np的nan
    return label_data


def data_translate(crime_data_continuity_range, date_range):
    """
    :param crime_data_continuity_range: DataFrame,3列，'date_str','grid_num','case_nums'(本格子是否有案件 或 外层格子是否有案件)
    :param date_range: Number,推移天数(相当于热点期天数)
    :return: DataFrame,多列,date_str索引,格子1,格子2,...,格子n
    """
    crime_data_continuity_range.columns = ['date_str', 'grid_num', 'value']
    trans_data = crime_data_continuity_range.pivot('date_str', 'grid_num', 'value')
    trans_data = trans_data.sort_index(axis=1).sort_index(axis=0)
    trans_data = trans_data.rolling(window=date_range)
    trans_data = trans_data.sum() / date_range
    trans_data = trans_data.shift(1)
    trans_data = trans_data.dropna(axis=0)

    return trans_data


def data_filter(trans_data):
    """
    :param trans_data: 转换后的数据
    :return: 返回过滤后的数据,列数可能会还少
    """
    boolean = trans_data.sum() > 0.0
    filter_data = trans_data[trans_data.columns[boolean]]
    return filter_data


def data_train_x(start_date, end_date, date_range, crime_data):
    """
    :param start_date: str,训练数据起始时间
    :param end_date: str,训练数据结束时间,左开右闭
    :param date_range: Number,推移天数(相当于热点期天数)
    :param crime_data: DataFrame,可能存在日期间断的案件数据
    :return: 用于建模的训练输入数据X
    """
    _, date_range_array, grid_array = date_grid(start_date, end_date, date_range, crime_data)
    train_x_data = data_continuity(date_range_array, grid_array, crime_data)
    train_x_data = data_translate(train_x_data, date_range)
    # train_x_data = data_filter(train_x_data)  # 列过滤
    return train_x_data


def data_train_y(start_date, end_date, date_range, crime_data):
    """
    :param start_date: str,训练数据起始时间
    :param end_date: str,训练数据结束时间,左开右闭
    :param date_range: Number,推移天数(相当于热点期天数)
    :param crime_data: DataFrame,可能存在日期间断的案件数据
    :return: 用于建模的训练输入数据Y
    """
    date_array, _, grid_array = date_grid(start_date, end_date, date_range, crime_data)
    train_y_data = data_continuity(date_array, grid_array, crime_data)
    train_y_data = data_label(train_y_data)
    train_y_data = train_y_data.grid_num
    return train_y_data


def data_test_x(end_date, date_range, crime_data):
    """
    :param end_date: str,训练数据结束时间,左开右闭
    :param date_range: Number,推移天数(相当于热点期天数)
    :param crime_data: DataFrame,可能存在日期间断的案件数据
    :return: 用于建模的验证输入数据X
    """
    test_end_date = pd.date_range(str(max(crime_data.date_str)), periods=1).shift(2).strftime('%Y%m%d')[0]
    test_x_data = data_train_x(end_date, test_end_date, date_range, crime_data)
    return test_x_data


def data_test_y(end_date, date_range, crime_data):
    """
    :param end_date: str,训练数据结束时间,左开右闭
    :param date_range: Number,推移天数(相当于热点期天数)
    :param crime_data: DataFrame,可能存在日期间断的案件数据
    :return: 用于建模的训练输入数据Y
    """
    test_end_date = pd.date_range(str(max(crime_data.date_str)), periods=1).shift(1).strftime('%Y%m%d')[0]
    test_y_data = data_train_y(end_date, test_end_date, date_range, crime_data)
    return test_y_data


def y_func(x, w, top_n):
    """
    :param x: DataFrame,训练数据X
    :param w: np.array,根据优化算法调整的权重值
    :param top_n: topN
    :return: np.array,取topN后的格子列表序列
    """
    y_mul = np.multiply(x, w)
    ind = np.apply_along_axis(lambda row: np.argsort(row)[-top_n:][::-1], axis=1, arr=y_mul)
    y_ = np.array(y_mul.columns[ind])
    return y_


def sort_loss(y_, y):
    """
    :param y_: np.array,取topN后的格子列表序列
    :param y: np.array,实际案发格子的列表序列
    :return: 实际案发各自数目; 命中格子数(注意与“命中案件数”的区别); 使得sum(1-p)最小
    """
    y_pred = y_.tolist()
    y_no_nan = list(map(lambda x: [] if np.isnan(x).all() else x, y))
    y_pred.extend(y_no_nan)

    case_count = np.sum(list(map(len, y_no_nan)))
    yy_ = np.array(y_pred).reshape((-1, 2), order='F')
    p_array = np.apply_along_axis(lambda x: len(set(x[0]).intersection(set(x[1])))/(len(set(x[1]))+1), 1, yy_)  # 加1,避免分母为0
    hit_count = np.sum(np.apply_along_axis(lambda x: len(set(x[0]).intersection(set(x[1]))), 1, yy_))
    return (case_count, hit_count), len(p_array) - np.sum(p_array)


def data_out(test_x_data, w):
    result = np.multiply(test_x_data, w)
    result.index.name = 'date_str'
    result = result.apply(lambda x: 0.001+0.099*(x-min(x))/(max(x)-min(x)+0.000001), axis=1)  # 分母加0.000001是为了避免nan值出现
    result.columns.name = 'grid_num'
    result = result.stack().reset_index(level=['date_str', 'grid_num'])
    result.rename(columns={0: 'p'}, inplace=True)
    result = result.sort_values(by=['date_str', 'grid_num'], ascending=[True, False])
    return result

