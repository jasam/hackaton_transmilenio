import timeit
import pandas as pd

our_str = 'F:/data/transmilenio/Datos/DATOS TRANSMILENIO/F1 Y F2 2011/MAYO/test.xlsx'
start_time = timeit.default_timer()
data_xls = pd.read_excel(our_str, sheetname=0)
new_f = our_str.replace('xlsx', 'csv')
data_xls.to_csv(new_f, encoding='utf-8')
print(new_f)
elapsed = timeit.default_timer() - start_time
print("time elapsed" + str(elapsed))  