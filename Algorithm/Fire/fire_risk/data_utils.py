#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
定义数据导入、数据转换、数据输出、关键计算相关的函数

"""

import json
import numpy as np
import pandas as pd

pd.set_option('display.float_format', lambda x: '%14.f' % x)


def read_config(config_path):
    with open(config_path + '/config.json', 'r') as f:
        config = json.load(f)
    file_train_path = config['file_train']
    file_predict_path = config['file_predict']
    model_path = config['model']
    deadline = config['deadline']
    output_path = config['output']
    push_rate = config['push']
    return file_train_path, file_predict_path, model_path, deadline, output_path, push_rate


def data_input_from_file(file_path):
    fire_data = pd.read_csv(file_path, header=0)  # 带表头
    return fire_data


def data_input_from_database(db_config):
    pass


class DataTranslate:
    def __init__(self, fire_data):
        self.fire_data = fire_data

    def if_else(self, col_name, sign_l_e_g, division_value):
        if sign_l_e_g == '>=':
            self.fire_data[col_name].where(self.fire_data[col_name] < division_value, division_value, inplace=True)
        if sign_l_e_g == '>':
            self.fire_data[col_name].where(self.fire_data[col_name] <= division_value, division_value, inplace=True)
        if sign_l_e_g == '==':
            self.fire_data[col_name + '_' + str(division_value)] = np.where(self.fire_data[col_name] == division_value, 1, 0)
        if sign_l_e_g == '<':
            self.fire_data[col_name].where(self.fire_data[col_name] >= division_value, division_value, inplace=True)
        return self

    def del_col(self, col_names):
        self.fire_data = self.fire_data.drop(col_names, axis=1)
        return self

    def fill_na(self, col_name, fill_value):
        self.fire_data[col_name] = self.fire_data[col_name].fillna(fill_value)
        return self

    def date_diff(self, col_name, deadline):
        self.fire_data[col_name+'_to_deadline'] = (pd.to_datetime(str(deadline)+'120000') -
                                                   pd.to_datetime(self.fire_data[col_name].apply(lambda x: str(x)[:14]))
                                                   ).apply(lambda x: x.days)
        return self

    def col_diff_if_else(self, col_name_left, col_name_right):
        self.fire_data[col_name_left+'_minus_'+col_name_right] = self.fire_data[col_name_left] - \
                                                                 self.fire_data[col_name_right]
        return self.if_else(col_name_left+'_minus_'+col_name_right, '<', 0)










