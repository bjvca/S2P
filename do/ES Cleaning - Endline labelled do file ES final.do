*Cleaning for outliers
use "C:\Users\ESiyame\Documents\S2P\IRB S2P\Local IRB\Field Docs\Write up\endline_labeled 2.dta", clear
replace test_plot1fert_time=. if test_plot1fert_time==98
replace test_plot1fert_app_meth=. if test_plot1fert_app_meth==98
replace test_plot1qty_fert=. if test_plot1qty_fert==999
replace test_plot1qty_fert = 250 if test_plot1qty_fert >250 & test_plot1qty_fert != .
replace test_plot1qty_fert = 5 if test_plot1qty_fert >0 & test_plot1qty_fert <5 & test_plot1qty_fert != .
replace test_plot1fert_cost=. if test_plot1fert_cost==999
replace test_plot1fert_cost=487500 if test_plot1fert_cost>487500 & test_plot1fert_cost !=.
replace test_plot1qty_fert_aip=100 if test_plot1qty_fert_aip>100 & test_plot1qty_fert_aip !=.
replace test_plot2qty_fert=5 if test_plot2qty_fert <5 & test_plot2qty_fert!=.
replace test_plot2qty_fert=200 if test_plot2qty_fert >200 & test_plot2qty_fert!=.
replace test_plot2fert_cost=. if test_plot2fert_cost==999
replace test_plot2fert_cost=3000 if test_plot2fert_cost<3000 & test_plot2fert_cost !=.
replace test_plot2fert_cost=440000 if test_plot2fert_cost >440000 & test_plot2fert_cost !=.
replace test_plot2qty_fert_aip=10 if test_plot2qty_fert_aip==1 & test_plot2qty_fert_aip !=.
replace test_plot2qty_fert_aip=100 if test_plot2qty_fert_aip==125 & test_plot2qty_fert_aip !=.
replace test_plot3qty_fert =250 if test_plot3qty_fert >250 & test_plot3qty_fert !=.
replace test_plot3fert_cost=. if test_plot3fert_cost==999
replace test_plotttl_cost_pest=. if test_plotttl_cost_pest==999
replace test_plotttl_cost_pest=150000 if test_plotttl_cost_pest>150000 &  test_plotttl_cost_pest!=.
replace test_plotplot_siz=. if test_plotplot_siz==999
replace test_plotplot_siz=5 if test_plotplot_siz>5 & test_plotplot_siz !=.
replace test_plotsoil_str=. if test_plotsoil_str==5
replace test_plotdist_plot=. if test_plotdist_plot==999
replace test_plotdist_plot=120 if test_plotdist_plot>120 & test_plotdist_plot !=.
replace test_plotseed_rec_times=. if test_plotseed_rec_times==999
replace test_plotseed_rec_times=6 if test_plotseed_rec_times>6 & test_plotseed_rec_times !=.
replace test_plotqty_used=. if test_plotqty_used==999
replace test_plotqty_used=0.002 if test_plotqty_used >0 & test_plotqty_used <0.002 & test_plotqty_used !=.
replace test_plotqty_used=50 if test_plotqty_used >50 & test_plotqty_used !=.
replace test_plotprice_seed=. if test_plotprice_seed==999
replace test_plotprice_seed=188000 if test_plotprice_seed>188000 & test_plotprice_seed !=.
replace test_plotttl_cost_pest=. if test_plotttl_cost_pest==999
replace test_plotttl_cost_pest=150000 if test_plotttl_cost_pest >150000 & test_plotttl_cost_pest !=.
replace test_plotbags_mcrp=. if test_plotbags_mcrp==999
replace test_plotbags_mcrp=4200 if test_plotbags_mcrp >4200 & test_plotbags_mcrp !=.
replace test_plotbags_sell=. if test_plotbags_sell==999
replace test_plotbags_sell=3 if test_plotbags_sell>3 & test_plotbags_sell<0 & test_plotbags_sell !=.
replace test_plotbags_sell=3 if test_plotbags_sell<3 & test_plotbags_sell>0 & test_plotbags_sell !=.
replace test_plotbags_sell=2270 if test_plotbags_sell >2270 & test_plotbags_sell !=.
replace test_plotprc_main=. if test_plotprc_main==0
replace test_plotprc_main=. if test_plotprc_main==999
replace  test_plotttl_exp=. if  test_plotttl_exp==999
replace test_plotprc_main=400 if test_plotprc_main <400 & test_plotprc_main >0 & test_plotprc_main !=.
replace test_plotprc_main=3000 if test_plotprc_main >3000 & test_plotprc_main !=.
