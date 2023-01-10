

global aird "SET GLOBAL HERE"
set scheme s1color

/************* legend

- time series on both types of exchanges

- cross sectional differences on DeFi

- airdrops and governance tokens

- clean airdrop data


****************/

*------------------------------ time series on both types of exchanges

use "$aird\data\exchange_volumes_coingecko.dta",clear
merge m:1 exchange_id using "$aird\data\exchanges_coingecko.dta"
keep if _merge==3
drop _merge
keep if year>=2019
gen volUSD_cent=volume_24h_usd if centralized==1
gen volUSD_defi=volume_24h_usd if centralized==0
gen nexch_cent=1 if centralized==1
gen nexch_defi=1 if centralized==0
collapse (sum) volUSD_* nexch_*,by(year month)

gen x=ym(year, month) // first convert a new var into year/month format
format x %tm // label it for stata to recognize sequencing

replace volUSD_cent=volUSD_cent/nexch_cent
replace volUSD_defi=volUSD_defi/nexch_defi

gen lvolUSD_cent=log10(volUSD_cent)
gen lvolUSD_defi=log10(volUSD_defi)

corr  lvolUSD_cent lvolUSD_defi
local temp: di %3.2f r(rho)
twoway (line lvolUSD_cent x, clcolor(dknavy) clwidth(thin) clpattern(solid) ) (line lvolUSD_defi x, clcolor(red*1.2) clwidth(thin) clpattern(dash) ) , xtitle("") legend( order( 1 "Centralized" 2 "Decentralized") ) ytitle("log(Total Volume in USD / # Exchanges)") // note(Correlation = `temp') 
graph export "$aird\drafts\figs\twoway_volume.pdf", as(pdf) replace


*------------------------------ cross sectional differences on DeFi
use "$aird\data\exchange_volumes_coingecko.dta",clear
collapse (sum) volume_24h_btc volume_24h_usd volume_24h_eth,by(exchange_id year month)
merge m:1 exchange_id using "$aird\data\exchanges_coingecko.dta"
keep if _merge==3
drop _merge
merge m:1 exchange_id using "$aird\data\year_exchange.dta"
keep if _merge==3
drop _merge
replace year_established=2019 if year_established==219
egen tid=group(year month)
egen eid=group(exchange_id)
gen decentralize=cond(centralized==0,1,0)
tsset eid tid
gen lvolume_24h_usd=asinh(volume_24h_usd)
gen dlvolume_24h_usd=(volume_24h_usd-L.volume_24h_usd)/L.volume_24h_usd
replace dlvolume_24h_usd=. if dlvolume_24h_usd==-1
winsor2 dlvolume_24h_usd,trim cuts(1 99) replace
gen lag1_lvolume_24h_usd=L1.lvolume_24h_usd
gen lag2_lvolume_24h_usd=L2.lvolume_24h_usd
gen lag1_dlvolume_24h_usd=L1.dlvolume_24h_usd
gen yrgrp=2011 if est_year_established==2011 | est_year_established==2012
replace yrgrp=2013 if est_year_established==2013
replace yrgrp=2014 if est_year_established==2014
replace yrgrp=2015 if est_year_established==2015 | est_year_established==2016
replace yrgrp=2017 if est_year_established==2017
replace yrgrp=2018 if est_year_established==2018
replace yrgrp=2019 if est_year_established==2019
replace yrgrp=2020 if est_year_established==2020 | est_year_established==2021
tab yrgrp,gen(yrdum)

*restrict to common sample
drop if lag1_dlvolume_24h_usd==. | lag1_lvolume_24h_usd==.
drop if trust_score_rank==. | year_established==.

global airX1 trust_score_rank year_established
global airX2 trust_score_rank yrdum2 yrdum3 yrdum4 yrdum5 yrdum6 yrdum7 yrdum8

reg lvolume_24h_usd decentralize,cluster(eid)
estadd local hastid "No"
est sto temp1
reg lvolume_24h_usd decentralize $airX1,cluster(eid)
estadd local hastid "No"
est sto temp2
areg lvolume_24h_usd decentralize $airX2,a(tid) vce(cl eid)
estadd local hastid "Yes"
est sto temp3
areg lvolume_24h_usd decentralize lag1_lvolume_24h_usd $airX2,a(tid) vce(cl eid)
estadd local hastid "Yes"
est sto temp4
reg dlvolume_24h_usd decentralize,cluster(eid)
estadd local hastid "No"
est sto temp5
reg dlvolume_24h_usd decentralize $airX1,cluster(eid)
estadd local hastid "No"
est sto temp6
areg dlvolume_24h_usd decentralize $airX2,a(tid) vce(cl eid)
estadd local hastid "Yes"
est sto temp7
areg dlvolume_24h_usd decentralize lag1_dlvolume_24h_usd $airX2,a(tid) vce(cl eid)
estadd local hastid "Yes"
est sto temp8

la var decentralize "Decentralized"
la var lag1_lvolume_24h_usd "log(Volume)$^{t-1}$"
la var lag1_dlvolume_24h_usd "(Volume Growth)$^{t-1}$"
la var trust_score_rank "Trust Score Rank"
la var year_established "Year Established"
la var yrdum2 "Established in 2013"
la var yrdum3 "Established in 2014"
la var yrdum4 "Established in 2015-16"
la var yrdum5 "Established in 2017"
la var yrdum6 "Established in 2018"
la var yrdum7 "Established in 2019"
la var yrdum8 "Established in 2020-21"
local tokeep "decentralize lag1_lvolume_24h_usd lag1_dlvolume_24h_usd trust_score_rank year_established yrdum2 yrdum3 yrdum4 yrdum5 yrdum6 yrdum7 yrdum8 _cons"
cd "$aird\drafts\tabs"
esttab temp1 temp2 temp3 temp4 temp5 temp6 temp7 temp8 using volume_central.tex, b(3) replace star(* 0.10 ** 0.05 *** 0.01) mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)") nonum brackets se mgroups("log(Volume in USD)" "Monthly Volume Growth", pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) label keep(`tokeep') order(`tokeep') stats (r2 N hastid, label("R-squared" "Sample Size" "Time FE") fmt(3 0)) parentheses nolz  nogaps fragment nolines 


*selection on unobservables test
areg dlvolume_24h_usd decentralize lag1_dlvolume_24h_usd $airX2,a(tid) vce(cl eid)
psacalc delta decentralize // assume the selection on unobservables = selection on observables
psacalc delta decentralize, rmax(0.06) // take this as 1.3* max r2


*------------------------------ airdrops and governance tokens

clear all
insheet using "$aird\data\panel_exchange_token_grouped_airdropdate.csv",clear
gen year=substr(date_x,1,4)
gen month=substr(date_x,6,2)
destring year month,replace
gen governance=cond(token_governance_token=="True",1,0)
gen exchangetoken=cond(token_exchange_token=="True",1,0)
gen lmarket_cap_usd=asinh(coin_market_cap_usd)
gen hasposmarketcap=cond(coin_market_cap_usd>0,1,0)
gen lvolume=asinh(coin_trading_volume_usd)
gen dex=cond(centralized=="False",1,0)
drop centralized
merge m:1 exchange_id using "$aird\data\exchanges_coingecko.dta"
keep if _merge==3 | _merge==1
drop _merge
merge m:1 exchange_id using "$aird\data\year_exchange.dta"
keep if _merge==3 | _merge==1
drop _merge
gen dex_governance=dex*governance
gen dex_lvolume=dex*lvolume
ren airdrop temp
gen airdrop=cond(temp=="True",1,0)
drop temp
gen dex_airdrop=dex*airdrop
gen airdrop_governance=airdrop*governance
egen tid=group(year month)
egen eid=group(exchange_id)
tsset eid tid
gen dlmarket_cap_usd=(coin_market_cap_usd-L.coin_market_cap_usd)/L.coin_market_cap_usd
replace dlmarket_cap_usd=. if coin_market_cap_usd==-1 | coin_market_cap_usd==0
gen dlvolume=(coin_trading_volume_usd-L.coin_trading_volume_usd)/L.coin_trading_volume_usd
replace dlvolume=. if coin_trading_volume_usd==-1 | coin_trading_volume_usd==0
winsor2 dlmarket_cap_usd dlvolume,replace trim cuts(1 97)

*create consistent sample
drop if est_year_established==. //| trust_score_rank==. //| year_established==.

tab year_established

gen yrgrp=2014 if est_year_established==2014 | est_year_established==2016
replace yrgrp=2017 if est_year_established==2017
replace yrgrp=2018 if est_year_established==2018
replace yrgrp=2019 if est_year_established==2019
replace yrgrp=2020 if est_year_established==2020 | est_year_established==2021
tab yrgrp,gen(yrdum)

*use airX2 since year_established has more missing obs
global airX1 trust_score_rank est_year_established
global airX2  yrdum2 yrdum3 yrdum4 yrdum5 //trust_score_rank

reg dlmarket_cap_usd dex governance $airX2 i.tid if hasposmarketcap==1,cluster(eid)
estadd local hasy "Yes",replace
estadd local sample "Pooled"
est sto temp11
reg dlmarket_cap_usd dex airdrop governance lvolume $airX2 i.tid if hasposmarketcap==1,cluster(eid)
estadd local hasy "Yes",replace
estadd local sample "Pooled"
est sto temp12
reg dlmarket_cap_usd dex airdrop dex_airdrop governance $airX2 lvolume i.tid if hasposmarketcap==1,cluster(eid)
estadd local hasy "Yes",replace
estadd local sample "Market Cap > 0"
est sto temp13
reg dlmarket_cap_usd dex airdrop governance airdrop_governance $airX2 lvolume i.tid if hasposmarketcap==1,cluster(eid)
estadd local hasy "Yes",replace
estadd local sample "Market Cap > 0"
est sto temp14
psacalc beta airdrop_governance
reg dlvolume dex governance $airX2 i.tid,cluster(eid)
estadd local hasy "Yes",replace
estadd local sample "Pooled"
est sto temp15
reg dlvolume dex airdrop governance $airX2 i.tid,cluster(eid)
estadd local hasy "Yes",replace
estadd local sample "Pooled"
est sto temp16
reg dlvolume dex airdrop dex_airdrop governance $airX2 i.tid,cluster(eid)
estadd local hasy "Yes",replace
estadd local sample "Pooled"
est sto temp17
reg dlvolume dex airdrop governance airdrop_governance $airX2 i.tid,cluster(eid)
estadd local hasy "Yes",replace
estadd local sample "Pooled"
est sto temp18


la var dex "Decentralized Exchange (DEX)" 
la var airdrop "Airdrop"
la var dex_airdrop "\quad $\times$ DEX"
la var governance "Governance Token"
la var airdrop_governance "\quad $\times$ Airdrop"
la var exchangetoken "Exchange Token"
la var lvolume "log(Volume)"
la var trust_score_rank "Trust Score Rank"
la var year_established "Year Established"
la var yrdum2 "Established in 2017"
la var yrdum3 "Established in 2018"
la var yrdum4 "Established in 2019"
la var yrdum5 "Established in 2020"
local tokeep "dex airdrop dex_airdrop governance airdrop_governance lvolume trust_score_rank yrdum2 yrdum3 yrdum4 yrdum5"
cd "$aird\drafts\tabs"
esttab temp11 temp12 temp13 temp14 temp15 temp16 temp17 temp18 using marketcap_tokens.tex, b(3) replace star(* 0.10 ** 0.05 *** 0.01) mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)" "(13)" "(14)") nonum brackets se mgroups("Market Capitalization (Growth)" "Volume (Growth)", pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) label keep(`tokeep') order(`tokeep') stats (r2 N hasy, label("R-squared" "Sample Size" "Time FE") fmt(2 0)) parentheses nolz  nogaps fragment nolines 


*selection on unobservables test - do 1.3*r2
reg dlmarket_cap_usd dex airdrop dex_airdrop governance $airX2 lvolume i.year i.month if hasposmarketcap==1,cluster(eid)
*psacalc delta dex_airdrop // assume the selection on unobservables = selection on observables
psacalc delta dex_airdrop,rmax(0.72)

reg dlmarket_cap_usd dex airdrop airdrop_governance governance $airX2 lvolume i.year i.month if hasposmarketcap==1,cluster(eid)
psacalc delta airdrop_governance,rmax(0.32)

*------------------------------ clean airdrop data

*https://www.coingecko.com/en/methodology
insheet using "$aird\data\year_exchange.csv",clear
save "$aird\data\year_exchange.dta",replace

insheet using "$aird\data\exchanges_coingecko.csv",clear
replace centralized="1" if centralized=="True"
replace centralized="0" if centralized=="False"
replace centralized="1" if centralized=="1.0"
replace centralized="0" if centralized=="0.0"
destring centralized,replace
keep centralized country exchange_id name trust_score trust_score_rank year_established
save "$aird\data\exchanges_coingecko.dta",replace

insheet using "$aird\data\exchange_volumes_coingecko.csv",clear
gen year=substr(date,1,4)
gen month=substr(date,6,2)
gen day=substr(date,-2,2)
destring year month day,replace
drop date
collapse (mean) volume_24h_btc volume_24h_usd volume_24h_eth,by(year month day exchange) // some odd instances where there are repeat obs
save "$aird\data\exchange_volumes_coingecko.dta",replace
egen tid=group(year month day)
egen uid=group(exchange)
tsset uid tid
gen dlvolume_24h_usd=(volume_24h_usd-L.volume_24h_usd)/L.volume_24h_usd
replace dlvolume_24h_usd=. if dlvolume_24h_usd==-1
winsor2 dlvolume_24h_usd,trim cuts(1 99) replace
export delimited using "$aird\data\exchange_volumes_coingecko_clean.csv", replace


*new extract
insheet using "$aird\data\new extract\exchanges_coingecko.csv",clear
keep year_established exchange_id centralized
gen dex=cond(centralized=="False",1,0)
save "$aird\data\new extract\exchanges_coingecko.dta",replace

insheet using "$aird\data\new extract\exchange_volumes.csv",clear
gen year=substr(date,1,4)
gen month=substr(date,6,2)
gen day=substr(date,-2,2)
destring year month day,replace
drop date
collapse (mean) volume_24h_btc,by(year month day exchange exchange_id) // some odd instances where there are repeat obs
merge m:1 exchange_id using "$aird\data\new extract\exchanges_coingecko.dta"
keep if _merge==3
drop _merge
drop if volume_24h_btc<0
drop if year==2018
gen volume_24h_btc_cex=volume_24h_btc if dex==0
gen volume_24h_btc_dex=volume_24h_btc if dex==1
collapse (sum) volume_24h_btc_*,by(year month)
gen volume_dex_share=volume_24h_btc_dex/(volume_24h_btc_dex+volume_24h_btc_cex)





