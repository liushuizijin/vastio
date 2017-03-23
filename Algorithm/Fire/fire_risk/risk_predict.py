"""
消防单位火灾风险预测
"""

import sys
import fire_risk.data_utils as fr
from sklearn.externals import joblib
# from sklearn import preprocessing as pc


# 读入配置参数
_, file_predict_path, model_path, deadline, output_path, push_rate = fr.read_config(sys.argv[1])
# 导入数据
fire_data = fr.data_input_from_file(file_predict_path)
# 对数据进行转换
fire_data = fr.DataTranslate(fire_data).if_else('aqsks', '>=', 10).if_else('dwdj', '==', 1).if_else('dwdj', '==', 2).\
    if_else('dwlx', '==', 1).if_else('dwlx', '==', 2).if_else('dwlx', '==', 3).if_else('dwxz', '==', 1).\
    if_else('dwxz', '==', 2).if_else('dwxz', '==', 3).if_else('dwxz', '==', 4).\
    del_col(['gdzc', 'ssqy', 'wgs84_x', 'wgs84_y', 'yyszdrs']).if_else('jcsl', '>', 100).\
    if_else('jzmj', '>', 10000).if_else('max_dscs', '>', 30).if_else('max_dsmj', '>', 10000).\
    if_else('max_dtsl', '>', 10).if_else('max_dxcs', '>', 3).if_else('max_dxmj', '>', 1000).\
    if_else('max_jzgd', '>', 100).if_else('max_jznl', '<', 0).if_else('max_jznl', '>', 10000).\
    if_else('max_rzsl', '>', 50).if_else('max_zdmj', '>', 10000).if_else('ssdts', '>', 10).if_else('xfcds', '>', 10).\
    if_else('xfdts', '>', 10).if_else('yhsl', '>', 300).if_else('zgrs', '>', 1000).if_else('zgsl', '>', 300).\
    fill_na('zjhzsj', 20010101000000).fill_na('zjjcsj', 20010101000000).if_else('zjyhsl', '>', 10).\
    if_else('zjzgsl', '>', 10).date_diff('zjhzsj', deadline).date_diff('zjjcsj', deadline).\
    col_diff_if_else('yhsl', 'zgsl').col_diff_if_else('zjyhsl', 'zjzgsl')
# 提取需要的数据列
fire_data = fire_data.fire_data.ix[:, ['dwmc', 'department', 'dwlx', 'deleted', 'dwid', 'aqsks', 'dwdj_1',
                                       'dwdj_2', 'dwxz_1', 'dwxz_2', 'dwxz_3', 'dwxz_4', 'hzsl', 'jcsl', 'jzmj',
                                       'jzsl', 'sfgpdw', 'sfzdyhdw', 'ssdts', 'xfcds', 'xfdts', 'yhsl', 'zdxfss',
                                       'zgrs', 'zjyhsl', 'zjzgsl', 'hzts_to_deadline', 'jcts_to_deadline',
                                       'yhsl_minus_zgsl', 'zjyhsl_minus_zjzgsl', 'zddw', 'ybdw', 'jxdw', 'wxp',
                                       'max_jzzt', 'max_jznl', 'max_jzgd', 'max_zdmj', 'max_dscs', 'max_dsmj',
                                       'max_dxcs', 'max_dxmj', 'max_nhdj', 'max_rnrs', 'max_dtsl', 'max_xfkzs',
                                       'max_rzsl', 'max_xfsssl']].fillna(0)
output = fire_data.ix[:, ['dwid', 'dwmc', 'department', 'dwlx', 'deleted']]
# 加载模型：弹性网络模型
# enet = joblib.load(model_path+'/fire_risk_model_enet.pkl')
# pred_enet = enet.predict(fire_data.ix[:, 5:])
# output['result_enet'] = pred_enet

# 加载模型：逻辑回归模型
lgr = joblib.load(model_path+'/fire_risk_model_lgr.pkl')
pred_lgr = lgr.predict_proba(fire_data.ix[:, 5:])[:, 1]
# pred_lgr = pc.MinMaxScaler((O, 0.1)).fit_transform(pred_lgr)
output['result_lgr'] = pred_lgr

topN = int(push_rate * output.shape[0])
output = output[output.deleted == 0]

output = output.sort_values(by='result_lgr', ascending=False)
output = output.ix[:topN, :]
output.to_csv(output_path + '/fire_risk_result_lgr.csv', index=False, header=True)
