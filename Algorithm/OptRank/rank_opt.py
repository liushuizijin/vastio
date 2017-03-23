#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Model name：COONN(Conditional optimal ordering based on Neural Network)
Vsersion：1.0
Details：条件最优排序算法
Library：numpy,pandas,TensorFlow
"""

import sys
# import tensorflow as tf
import scipy.optimize as opt
import data_utils

# 获取脚本参数
# pd_id = sys.argv[1]
# data_in_path = sys.argv[2]
# model_path = sys.argv[3]
# start_date = sys.argv[4]
# end_date = sys.argv[5]
# date_range = int(sys.argv[6])
# top_n = sys.argv[7]
# data_out_path = sys.argv[8]
pd_id, data_in_path, model_path, start_date, end_date, date_range, top_n, data_out_path = data_utils.read_config(
    sys.argv[1])

# 加载数据
crime_data, class_count = data_utils.data_input(data_in_path)


def rank_opt_with_tf(class_, crime_, start_, end_, date_, pd_):
    # 逐个班次训练计算
    for i in range(1, class_ + 1):
        train_x_data = data_utils.data_train_x(start_, end_, date_, crime_.ix[:, [0, 1, 2 * i]])
        train_y_data = data_utils.data_train_y(start_, end_, date_, crime_.ix[:, [0, 1, 2 * i]]).values
        test_x_data = data_utils.data_test_x(end_, date_, crime_.ix[:, [0, 1, 2 * i]])
        logical_test_y = max(train_x_data.index) > int(end_)
        if logical_test_y:  # 验证数据Y
            test_x_data = test_x_data.iloc[:-1, ]
            test_y_data = data_utils.data_test_y(end_, date_, crime_.ix[:, [0, 1, 2 * i]]).values
        # 用于feed的输入数据：
        # X: train_x_data (DataFrame)
        # Y: train_y_data (array)

        # 构造排序模型
        X_shape = train_x_data.shape
        # k = tf.constant(10)  # topN
        X = tf.placeholder(tf.float32, X_shape)
        Y = tf.placeholder(tf.float32, [X_shape[0], 1])

        W = tf.Variable(tf.ones([1, X_shape[1]]))
        Y_ = data_utils.y_func(X, W, top_n)  # X是DataFrame而不是array！待验！

        # 定义损失函数,优化方法
        stat, loss = data_utils.sort_loss(Y_, Y)
        optimizer = tf.train.GradientDescentOptimizer(0.01).minimize(loss)

        # 初始化变量
        init = tf.initialize_all_variables()

        # 定义保存model的内容
        # saver = tf.train.Saver()

        # 运行图计算
        with tf.Session() as sess:
            # 运行初始化
            sess.run(init)

            # 执行计算
            for step in range(5000):
                if (step + 1) % 100 == 0:
                    ct, ls = sess.run([stat, loss], feed_dict={X: train_x_data, Y: train_y_data})
                    print("Step:", "%04d" % (step + 1), "case_count=", "%03d" % (ct[0]),
                          "hit_count=", "%03d" % (ct[1]), "loss=", "{:.9f}".format(ls))
            print("Optimization Finished")
            train_stat = sess.run(stat, feed_dict={X: train_x_data, Y: train_y_data})
            print("训练期总案发格子数 =", train_stat[0], "训练期总命中格子数 =", train_stat[1],
                  "命中率 =", train_stat[1] / train_stat[0])

            w = sess.run(W)
            test_y_ = data_utils.y_func(test_x_data, w, top_n)
            if logical_test_y:
                test_stat, _ = data_utils.sort_loss(test_y_, test_x_data.values)
                print("验证期总案发格子数 =", test_stat[0], "验证期总命中格子数 =", test_stat[1],
                      "命中率 =", test_stat[1] / test_stat[0], "\n")

            result = data_utils.data_out(test_x_data, w)
            if class_ == 1:
                result.to_csv(data_out_path + "/pred_result/noclass_" + pd_ + "_OptRank.csv", index=False, header=False)
            else:
                result.to_csv(data_out_path + "/pred_result/class_" + str(i) + "_" + pd_ + "_OptRank.csv", index=False,
                              header=False)
                # if class_ == 1:
                #     save_path = saver.save(sess, model_path+"/pred_result/"+pd_+"_0_OptRank.ckpt")
                # else:
                #     save_path = saver.save(sess, model_path+"/pred_result/"+pd_+"_"+str(i)+"_OptRank.ckpt")
                # print("Model saved in file: %s" % model_path)
    return None


def rank_opt_with_sci(class_, crime_, start_, end_, date_, pd_):
    # 逐个班次训练计算
    for i in range(1, class_ + 1):
        train_x_data = data_utils.data_train_x(start_, end_, date_, crime_.ix[:, [0, 1, 2 * i]])
        train_y_data = data_utils.data_train_y(start_, end_, date_, crime_.ix[:, [0, 1, 2 * i]]).values
        test_x_data = data_utils.data_test_x(end_, date_, crime_.ix[:, [0, 1, 2 * i]])
        logical_test_y = max(test_x_data.index) > int(end_)
        if logical_test_y:  # 验证数据Y
            test_x_data = test_x_data.iloc[:-1, ]
            test_y_data = data_utils.data_test_y(end_, date_, crime_.ix[:, [0, 1, 2 * i]]).values
        # 用于feed的输入数据：
        # X: train_x_data (DataFrame)
        # Y: train_y_data (array)

        # 构造排序模型
        X_shape = train_x_data.shape
        init_w = [1.0] * X_shape[1]

        # res = opt.minimize(fun=lambda w: data_utils.sort_loss(data_utils.y_func(train_x_data, w, top_n), train_y_data)[1],
        #                    x0=init_w, method='CG', options={'disp': True, 'gtol': 0.01, 'maxiter': 1000, 'eps': 0.01})
        # res = opt.basinhopping(lambda w: data_utils.sort_loss(data_utils.y_func(train_x_data, w, top_n), train_y_data)[1],
        #                        init_w, niter=1000, stepsize=0.1)
        res = opt.brute(lambda w: data_utils.sort_loss(data_utils.y_func(train_x_data, w, top_n), train_y_data)[1],
                        ((-1,1,0.0001),)*X_shape[1], finish=None)
        
        # print(res.x)
        print(res)
        # train_stat = data_utils.sort_loss(data_utils.y_func(train_x_data, res.x, top_n), train_y_data)[0]
        train_stat = data_utils.sort_loss(data_utils.y_func(train_x_data, res, top_n), train_y_data)[0]
        print("训练期总案发格子数 =", train_stat[0], "训练期总命中格子数 =", train_stat[1],
              "命中率 =", train_stat[1] / train_stat[0])
        if logical_test_y:
            # test_stat = data_utils.sort_loss(data_utils.y_func(test_x_data, res.x, top_n), test_y_data)[0]
            test_stat = data_utils.sort_loss(data_utils.y_func(test_x_data, res, top_n), test_y_data)[0]
            print("验证期总案发格子数 =", test_stat[0], "验证期总命中格子数 =", test_stat[1],
                  "命中率 =", test_stat[1] / test_stat[0], "\n")
        # result = data_utils.data_out(test_x_data, res.x)
        result = data_utils.data_out(test_x_data, res)
        if class_ == 1:
            result.to_csv(data_out_path + "/pred_result/noclass_" + pd_ + "_OptRank.csv", index=False, header=False)
        else:
            result.to_csv(data_out_path + "/pred_result/class_" + str(i) + "_" + pd_ + "_OptRank.csv", index=False,
                          header=False)

if __name__ == '__main__':
    # rank_opt_with_tf(class_count, crime_data, start_date, end_date, date_range, pd_id)
    rank_opt_with_sci(class_count, crime_data, start_date, end_date, date_range, pd_id)

