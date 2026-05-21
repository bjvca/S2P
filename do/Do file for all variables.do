 use "C:\Users\ESiyame\Documents\S2P\IRB S2P\Local IRB\Field Docs\Write up\endli
> ne_labeled.dta" 
*Cleaning for all variables
replace hh_size=2 if hh_size <2 & hh_size !=.
replace hh_size=10 if hh_size >10 & hh_size !=.
replace hh_age =. if hh_age==999
replace hh_age=82 if hh_age >82 & hh_age !=.
replace hh_age=22 if hh_age <22 & hh_age !=.
replace ttl_area=. if ttl_area ==999
replace ttl_area=.5 if ttl_area <.5 & ttl_area !=.
replace ttl_area=10 if ttl_area >10 & ttl_area !=.
replace ttl_area_t_1=. if ttl_area_t_1==999
replace ttl_area_t_1=.5 if ttl_area_t_1<.5 & ttl_area_t_1 !=.
replace ttl_area_t_1 =11 if ttl_area_t_1 >11 & ttl_area_t_1 !=.
replace dist_agro =. if dist_agro==999
replace dist_agro=10 if dist_agro <10 & dist_agro !=.
replace dist_agro=360 if dist_agro >360 & dist_agro !=.
replace dist_fws=. if dist_fws==999
replace dist_fws =7 if dist_fws <7 & dist_fws !=.
replace dist_fws =720 if dist_fws >720 & dist_fws !=.
replace times_vis=. if times_vis ==999
replace times_vis=5 if times_vis >5 & times_vis !=.
replace times_ext=. if times_ext==999
replace times_ext=6 if times_ext >6 & times_ext !=.
replace times_ext=1 if times_ext <1 & times_ext !=.
replace test_plotplot_siz=. if test_plotplot_siz==999
replace test_plotplot_siz=.5 if test_plotplot_siz <.5 & test_plotplot_siz!=.
replace test_plotplot_siz=5 if test_plotplot_siz >5 & test_plotplot_siz!=.
replace test_plotdist_plot=. if test_plotdist_plot==999
replace test_plotdist_plot=120 if test_plotdist_plot>120 & test_plotdist_plot!=.
replace test_plotperc_main=10 if test_plotperc_main <10 & test_plotperc_main!=.
replace test_plotseed_rec_times=. if test_plotseed_rec_times==999
replace test_plotseed_rec_times=6 if test_plotseed_rec_times >6 & test_plotseed_rec_times !=.
replace test_plotqty_used=. if test_plotqty_used==999
replace test_plotqty_used=50 if test_plotqty_used >50 & test_plotqty_used !=.
replace test_plotprice_seed=. if test_plotprice_seed==999
replace test_plotprice_seed =188000 if test_plotprice_seed >188000 & test_plotprice_seed !=.
replace test_plot1qty_fert=. if test_plot1qty_fert==999
replace test_plot1qty_fert =5 if test_plot1qty_fert <5 & test_plot1qty_fert !=.
replace test_plot1qty_fert =250 if test_plot1qty_fert >250 & test_plot1qty_fert !=.
replace test_plot1fert_cost=. if test_plot1fert_cost==999
replace test_plot1fert_cost=487500 if test_plot1fert_cost >487500 & test_plot1fert_cost !=.
replace test_plot1qty_fert_aip=100 if test_plot1qty_fert_aip>100 & test_plot1qty_fert_aip !=.
replace test_plot2qty_fert=5 if test_plot2qty_fert <5 & test_plot2qty_fert !=.
replace test_plot2qty_fert=200 if test_plot2qty_fert >200 & test_plot2qty_fert !=.
replace test_plot2fert_cost=. if test_plot2fert_cost==999
replace test_plot2fert_cost=41 if test_plot2fert_cost<41 & test_plot2fert_cost !=.
replace test_plot2fert_cost=440000 if test_plot2fert_cost>440000 & test_plot2fert_cost !=.
replace test_plot2qty_fert_aip=1 if test_plot2qty_fert_aip<1 & test_plot2qty_fert_aip !=.
replace test_plot2qty_fert_aip=100 if test_plot2qty_fert_ai >100 & test_plot2qty_fert_aip !=.
replace test_plot3fert_cost=. if test_plot3fert_cost==999
replace test_plotttl_cost_pest=. if test_plotttl_cost_pest==999
replace test_plotttl_cost_pest=150000 if test_plotttl_cost_pest >150000 & test_plotttl_cost_pest !=.
replace test_plotbags_mcrp=. if test_plotbags_mcrp==999
replace test_plotbags_mcrp=5 if test_plotbags_mcrp<5 & test_plotbags_mcrp!=.
replace test_plotbags_mcrp=4200 if test_plotbags_mcrp>4200 & test_plotbags_mcrp!=.
replace test_plotbags_sell=. if test_plotbags_sell==999
replace test_plotbags_sell=3 if test_plotbags_sell <3 & test_plotbags_sell!=.
replace test_plotbags_sell=2270 if test_plotbags_sell >2270 & test_plotbags_sell!=.
replace test_plotprc_main=400 if test_plotprc_main<400 & test_plotprc_main!=.
replace test_plotprc_main=. if test_plotprc_main==999
replace test_plotttl_exp=. if test_plotttl_exp==999
replace test_plotttl_exp=3000 if test_plotttl_exp <3000 & test_plotttl_exp !=.
replace test_plotttl_exp=1000000 if test_plotttl_exp >1000000 & test_plotttl_exp !=.
replace no_plots=4 if no_plots>4 & no_plots !=.
replace rnd_plotplot_sizr=. if rnd_plotplot_sizr==999
replace rnd_plotplot_sizr=.25 if rnd_plotplot_sizr<.25 & rnd_plotplot_sizr!=.
replace rnd_plotplot_sizr=4 if rnd_plotplot_sizr>4 & rnd_plotplot_sizr!=.
replace rnd_plotdist_plotr=. if rnd_plotdist_plotr==999
replace rnd_plotdist_plotr =180 if rnd_plotdist_plotr >180 & rnd_plotdist_plotr !=.
replace rnd_plotseed_rec_timesr=. if rnd_plotseed_rec_timesr==999
replace rnd_plotseed_rec_timesr=8 if rnd_plotseed_rec_timesr>8 & rnd_plotseed_rec_timesr !=.
replace rnd_plotqty_usedr=. if rnd_plotqty_usedr==999
replace rnd_plotqty_usedr=.001 if rnd_plotqty_usedr<.001 & rnd_plotqty_usedr !=.
replace rnd_plotqty_usedr=150 if rnd_plotqty_usedr>150 & rnd_plotqty_usedr !=.
replace rnd_plotprice_seedr=. if rnd_plotprice_seedr==999
replace rnd_plotprice_seedr=. if rnd_plotprice_seedr==99
replace rnd_plotprice_seedr=150000 if rnd_plotprice_seedr>150000 & rnd_plotprice_seedr !=.
replace rnd_plot1qty_fertr=. if rnd_plot1qty_fertr==999
replace rnd_plot1qty_fertr=2 if rnd_plot1qty_fertr<2 & rnd_plot1qty_fertr!=.
replace rnd_plot1qty_fertr=350 if rnd_plot1qty_fertr>350 & rnd_plot1qty_fertr!=.
replace rnd_plot1fert_costr=. if rnd_plot1fert_costr==999
replace rnd_plot1fert_costr=840000 if rnd_plot1fert_costr>840000 & rnd_plot1fert_costr !=.
replace rnd_plot2qty_fertr=. if rnd_plot2qty_fertr==999
replace rnd_plot2qty_fertr=2 if rnd_plot2qty_fertr<2 & rnd_plot2qty_fertr !=.
replace rnd_plot2qty_fertr=400 if rnd_plot2qty_fertr>400 & rnd_plot2qty_fertr !=.
replace rnd_plot2fert_costr=. if rnd_plot2fert_costr ==999
replace rnd_plot2fert_costr=990000 if rnd_plot2fert_costr >990000 & rnd_plot2fert_costr!=.
replace rnd_plotttl_cost_pestr=. if rnd_plotttl_cost_pestr==999
replace rnd_plotttl_cost_pestr=100000 if rnd_plotttl_cost_pestr>100000 & rnd_plotttl_cost_pestr !=.
replace rnd_plotbags_mcrpr=. if rnd_plotbags_mcrpr==999
replace rnd_plotbags_mcrpr=4560 if rnd_plotbags_mcrpr >4560 & rnd_plotbags_mcrpr!=.
replace rnd_plotbags_sellr=. if rnd_plotbags_sellr==999
replace rnd_plotbags_sellr=. if rnd_plotbags_sellr==99
replace rnd_plotbags_sellr=10 if rnd_plotbags_sellr<10 & rnd_plotbags_sellr !=.
replace rnd_plotbags_sellr=10 if rnd_plotbags_sellr<10 & rnd_plotbags_sellr !=.
replace rnd_plotbags_sellr=1550 if rnd_plotbags_sellr>1550 & rnd_plotbags_sellr !=.
replace rnd_plotprc_mainr=. if rnd_plotprc_mainr==999
replace rnd_plotprc_mainr=240 if rnd_plotprc_mainr <240 & rnd_plotprc_mainr !=.
replace rnd_plotprc_mainr=1000000 if rnd_plotprc_mainr >1000000 & rnd_plotprc_mainr !=.
replace rnd_plotttl_expr =. if rnd_plotttl_expr==999
replace rnd_plotttl_expr=3000 if rnd_plotttl_expr <3000 & rnd_plotttl_expr !=.
replace rnd_plotttl_expr=850000 if rnd_plotttl_expr >850000 & rnd_plotttl_expr !=.























































