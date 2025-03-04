from CosinorPy import file_parser, cosinor, cosinor1
import pandas as pd

#Read Data
df = file_parser.read_excel(r"C:\")

#Cosinor analysis：Identify the best models and/or the best periods
 ##(possible periods can be given as an interval or as a single value).
df_results = cosinor.fit_group(df, n_components = [1,2,3], period=24,
                               folder=r"C:\")
#Get the best fitting periods with criterium 'RSS'
df_best_fits = cosinor.get_best_fits(df_results, n_components = [1,2,3], criterium='RSS', reverse = False)
df_best_fits.to_csv("supp_table_1.csv", index=False)
#Get the best models based on p-value
df_best_models = cosinor.get_best_models(df, df_results, n_components = [1,2,3])
cosinor.plot_df_models(df, df_best_models,
                       folder=r"C:\")
df_best_models.to_csv("supp_table_2.csv", index=False)

#Cosinor1 analysis
df_results = cosinor1.fit_group(df, period=[24],
                                save_folder=r"C:\")
df_results.to_csv("supp_table_3.csv", index=False)

#Comparison analysis
pairs = (["test1", "test4"],["test1", "test4"])
cosinor1.test_cosinor_pairs = cosinor1.test_cosinor_pairs(df, pairs, period=24, folder=r"C:\")
cosinor1.test_cosinor_pairs.to_csv("test 4.csv", index=False)

#Use an 1-component cosinor for first pair
df_cosinor_lm1 = cosinor.compare_pairs_limo(df, pairs[:1], n_components = 1, period = 24, folder=r"C:\")
df_cosinor_lm1.to_csv("supp_test 5.csv", index=False)
