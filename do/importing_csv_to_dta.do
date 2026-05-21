 * Import & labeling with lowercase names; yes=1/no=0 for yes-no lists
do master.do 
import delimited using "$raw/endline.csv", varnames(1) stringcols(_all) bindquotes(strict) clear
*-------------------------------------------------------------*
* Rename ONLY v<number> vars using their labels:
* new name = <firstPart><number><lastPart>  (NO separators)
*-------------------------------------------------------------*

renvars test_plotfertilizer_repeat1ferti test_plotfertilizer_repeat2ferti test_plotfertilizer_repeat3ferti test_plotfertilizer_repeat4ferti ///
		rnd_plotfertilizer_repeatr1ferti rnd_plotfertilizer_repeatr2ferti rnd_plotfertilizer_repeatr3ferti / ///
		v129 v140 v151 v162 v290 v301 v312

local taken ""

foreach v of varlist v* {
    // only names like v130, v2, v999...
    if !regexm("`v'", "^v[0-9]+$") continue

    local lab : variable label `v'
    if "`lab'" == "" continue

    // Replace dots with spaces; multiple dots -> multiple spaces (tokenize ignores extras)
    local parts = subinstr("`lab'", ".", " ", .)

    // Extract first token, first numeric token, and last non-empty token
    local first ""
    local num ""
    local last ""

    // tokenize splits on spaces; consecutive spaces are fine
    tokenize "`parts'"

    // walk tokens
    local i = 1
    while "``i''" != "" {
        // set first token once
        if "`first'" == "" {
            local first "``i''"
        }
        // capture the first pure number token (e.g., 2, 12)
        if "`num'" == "" & regexm("``i''","^[0-9]+$") {
            local num "``i''"
        }
        // keep updating last with the most recent non-empty token
        if "``i''" != "" {
            local last "``i''"
        }
        local ++i
        if `i' > 200 { // safety
            continue, break
        }
    }

    // if no number found, you can either skip or use only first+last
    if "`num'" == "" {
        // Uncomment next line to skip when no number is present:
        // continue
        // Or: proceed with only first+last by setting num empty (as requested earlier we use number; here it may be absent)
    }

    // Build candidate with NO separators
    local cand = lower("`first'`num'`last'")
    // Sanitize and enforce 32-char limit
    local cand = strtoname("`cand'")
    local cand = substr("`cand'", 1, 32)

    // Ensure uniqueness (append _1, _2, …) and avoid no-op rename
    local base = "`cand'"
    local newname = "`cand'"
    local k = 1
    while (strpos(" `taken' ", " `newname' ") | "`newname'" == "`v'") {
        local suf = "_`k'"
        local base_trunc = substr("`base'", 1, 32 - length("`suf'"))
        local newname = "`base_trunc'`suf'"
        local ++k
        if `k' > 9999 {
            di as err "Could not find unique name for `v' (label: `lab')"
            continue, break
        }
    }

    di as txt "Renaming: `v' -> `newname'"
    rename `v' `newname'
    local taken "`taken' `newname'"
}

describe, short

foreach v of varlist _all {
    replace `v' = "" if inlist(lower(`v'), "n/a","na","nan")
    replace `v' = "" if trim(`v')==""
}

* Map select_one vars
cap label drop vl_q1
label define vl_q1 1 "DOWA" 2 "KASUNGU" 3 "MCHINJI" 4 "NTCHISI"
ds *q1, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __q1 = .
            replace __q1 = 1 if trim(`v') == "DOWA"
            replace __q1 = 2 if trim(`v') == "KASUNGU"
            replace __q1 = 3 if trim(`v') == "MCHINJI"
            replace __q1 = 4 if trim(`v') == "NTCHISI"
            drop `v'
            rename __q1 `v'
            label values `v' vl_q1
            label variable `v' "DISTRICT:"
    }
}
ds *q4a, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
}
* ===== Handle failed_crop* variables exported as separate columns with "True"/"False" =====
ds failed_crop*, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno

        * Derive a readable crop name from the variable name (remove prefix and underscores)
        local _crop = subinstr("`v'","failed_crop","",.)
        local _crop = subinstr("`_crop'","_"," ",.)

        * Use the known question text for failed_crop
local __lbl "[`_crop'] Q151. Which crop was affected?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}

ds *expensesr98, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[other] Q87c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *expensesr4, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[Transport for inputs and/or getting commodities to the market] Q149c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *expensesr3, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[Land preparation and/or rental] Q149c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *expensesr2, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[Labor Costs] Q149c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}


* ===== Handle rnd_plotfertilizer_listr* variables ("True"/"False") via suffix matching =====
local listr_suffixes listrmap listrdap listrurea listrcan listrnpk14142 listrnpk23105 ///
listrnpk81815 listrnpk15231 listrsop listrmop listrcalcitic listrdolomiti listrpotassiu ///
listrother listrdk listrnever ///
listmap listdap listurea listcan listnpk14142 listnpk23105 listnpk81815 listnpk15231 ///
listsop listmop listcalcitic listdolomiti listpotassiu listother listdk listnever

foreach s of local listr_suffixes {
    ds *`s', has(type string)
    if "`r(varlist)'" != "" {
        foreach v of varlist `r(varlist)' {
            capture confirm string variable `v'
            if !_rc {
                gen byte __tmp = .
                replace __tmp = 1 if lower(trim(`v')) == "true"
                replace __tmp = 0 if lower(trim(`v')) == "false"
                drop `v'
                rename __tmp `v'
            }
            label define yesno 0 "No" 1 "Yes", replace
            label values `v' yesno

           * Derive choice label from suffix (handles both listmap and listrmap, etc.)
local __suf "map dap urea can npk14142 npk23105 npk81815 npk15231 sop mop calcitic dolomiti potassiu other dk never"
local __lbl `" "MAP" "DAP" "UREA" "CAN" "NPK 14:14:2" "NPK 23:10:5" "NPK 8:18:15" "NPK 15:23:1" "SOP" "MOP" "Calcitic" "Dolomitic" "Potassium" "Other" "Don't know" "Never" "'

local ch ""
forvalues i = 1 / `: word count `__suf'' {
    local suf : word `i' of `__suf'
    local lab : word `i' of `__lbl'
    if "`ch'"=="" & regexm("`v'","list(r)?`suf'$") {
        local ch "`lab'"
    }
}

local __lbl "[`ch'] Q119. Which of the following fertilizers did you use on ${plot_select_name} plot in the most recent agricultural season of 2024/2025?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
        }
    }
}



ds *impr_nor96, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[OTHER REASONS] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}
ds *impr_nor11, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[SEED IS NOT WEED RESISTANT] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_nor10, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[TASTE IS NOT PREFERRED OR COOKING QUALITY IS POOR] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_nor9, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[LACK OF ACCESS TO OTHER COMPLEMENTARY INPUTS SUCH AS FERTILIZERS] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_nor8, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[LACK OF CASH/LOANS TO PAY UPFRONT] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_nor7, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[UNABLE TO ACCESS IMPROVED VARIETY I WANTED] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_nor6, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[QUALITY NOT TRUSTED/FEAR OF FAKE SEED] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_nor5, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[FEAR OF LOSS OF HARVEST//DISEASE] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_nor4, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[FEAR OF LOSS OF HARVEST/DROUGHT/WATER SHORTAGE] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_nor3, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[IMPROVED SEED YIELD IS LOW] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_nor2, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[PLOT/SOIL NOT SUITABLE FOR IMPROVED VARIETY] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}

* One helper that applies to both prim_resXX and prim_resrXX
capture program drop __primres_one
program define __primres_one
    // usage: __primres_one <code> "<option text>"
    args code opttext

    // pass 1: no "r"
    ds *prim_res`code', has(type string)
    if "`r(varlist)'" != "" {
        foreach v of varlist `r(varlist)' {
            capture confirm string variable `v'
            if !_rc {
                gen byte __tmp = .
                replace __tmp = 1 if lower(trim(`v')) == "true"
                replace __tmp = 0 if lower(trim(`v')) == "false"
                drop `v'
                rename __tmp `v'
            }
            label define yesno 0 "No" 1 "Yes", replace
            label values `v' yesno
            local __lbl "[`opttext'] Q111. What are the main reasons for your level of satisfaction with the seed variety?"
            local __lbl : display substr("`__lbl'", 1, 80)
            label variable `v' `"`__lbl'"'
        }
    }

    // pass 2: with "r"
    ds *prim_resr`code', has(type string)
    if "`r(varlist)'" != "" {
        foreach v of varlist `r(varlist)' {
            capture confirm string variable `v'
            if !_rc {
                gen byte __tmp = .
                replace __tmp = 1 if lower(trim(`v')) == "true"
                replace __tmp = 0 if lower(trim(`v')) == "false"
                drop `v'
                rename __tmp `v'
            }
            label define yesno 0 "No" 1 "Yes", replace
            label values `v' yesno
            local __lbl "[`opttext'] Q111. What are the main reasons for your level of satisfaction with the seed variety?"
            local __lbl : display substr("`__lbl'", 1, 80)
            label variable `v' `"`__lbl'"'
        }
    }
end
quietly __primres_one  1 "IMPROVED VARIETY IS TOO EXPENSIVE"
quietly __primres_one  2 "PREFERENCE FOR TRADITIONAL VARIETY"
quietly __primres_one  3 "HIGH-YIELDING"
quietly __primres_one  4 "EARLY MATURITY"
quietly __primres_one  5 "PROFITABLE COMPARED TO OTHER VARIETIES"
quietly __primres_one  6 "DROUGHT/HEAT RESISTANT"
quietly __primres_one  8 "INSECT/PEST RESISTANT"
quietly __primres_one  9 "DISEASE RESISTANT"
quietly __primres_one 10 "WEED RESISTANT"
quietly __primres_one 11 "LODGING RESISTANT"
quietly __primres_one 12 "TOLERANT TO SOIL TYPE"
quietly __primres_one 13 "NO OR MINIMAL USE OF FERTILIZER"
quietly __primres_one 14 "GOOD TASTE"
quietly __primres_one 15 "GOOD COOKING QUALITIES"
quietly __primres_one 16 "GOOD NUTRITIONAL QUALITIES"
quietly __primres_one 17 "POSSIBILITY OF SAVING SEEDS"
quietly __primres_one 18 "SEEDS EASY TO GET COMPARED TO OTHER VARIETIES"
quietly __primres_one 19 "AFFORDABLE PRICE OF SEEDS COMPARED TO OTHER VARIETIES"
quietly __primres_one 20 "HAD SEEDS FROM PREVIOUS SEASON"
quietly __primres_one 21 "ALWAYS USED THIS VARIETY"
quietly __primres_one 22 "ADVICE FROM EXTENSION OFFICER"
quietly __primres_one 23 "ADVICE FROM INPUT SUPPLIER"
quietly __primres_one 24 "ADVICE FROM FELLOW FARMER"
quietly __primres_one 26 "SEED RECEIVED THROUGH THE VOUCHER PROGRAM"
quietly __primres_one 27 "MORE OR HIGHER QUALITY FODDER"
quietly __primres_one 28 "LESS NEED FOR FERTILIZER OR MORE YIELD AT LOWER LEVELS OF FERTILIZER"
quietly __primres_one 29 "LESS WORK & DRUDGERY FOR PLANTING, WEEDING, HARVESTING ETC"
quietly __primres_one 96 "OTHER"




ds *expenses98, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[other] Q87c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *expenses4, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[Transport for inputs and/or getting commodities to the market] Q87c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *expenses3, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[Land preparation and/or rental] Q87c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *expenses2, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[Labor Costs] Q87c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}





ds *impr_no96, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[OTHER REASONS] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no11, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[SEED IS NOT WEED RESISTANT] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no10, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[TASTE IS NOT PREFERRED OR COOKING QUALITY IS POOR] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no9, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[LACK OF ACCESS TO OTHER COMPLEMENTARY INPUTS SUCH AS FERTILIZERS] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no8, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[LACK OF CASH/LOANS TO PAY UPFRONT] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no7, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[UNABLE TO ACCESS IMPROVED VARIETY I WANTED] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no6, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[QUALITY NOT TRUSTED/FEAR OF FAKE SEED] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no5, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[FEAR OF LOSS OF HARVEST//DISEASE] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no4, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[FEAR OF LOSS OF HARVEST/DROUGHT/WATER SHORTAGE] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no3, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[IMPROVED SEED YIELD IS LOW] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *impr_no2, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[PLOT/SOIL NOT SUITABLE FOR IMPROVED VARIETY] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *stick98, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[other reason] Q15c. What is the main reason you prefer to stick to your current fertilizer practice?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *stick8, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[I got a different fertilizer from AIP or other organizations or persons which I use] Q15c. What is the main reason you prefer to stick to your current fertilizer practice?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *stick7, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[My agro-dealer/extension agent recommended otherwise] Q15c. What is the main reason you prefer to stick to your current fertilizer practice?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *stick6, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[I want to see if it works for others first] Q15c. What is the main reason you prefer to stick to your current fertilizer practice?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *stick5, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[I don’t understand the recommendation well] Q15c. What is the main reason you prefer to stick to your current fertilizer practice?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *stick4, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[The recommended fertilizer is not easily available] Q15c. What is the main reason you prefer to stick to your current fertilizer practice?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *stick3, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[The recommended fertilizer is too expensive or not affordable] Q15c. What is the main reason you prefer to stick to your current fertilizer practice?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}



ds *stick2, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[I am satisfied with my current yields] Q15c. What is the main reason you prefer to stick to your current fertilizer practice?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}





cap label drop vl_reason
label define vl_reason 12 "Respondent permanently moved" 3 "Respondent died" 4 "Resopndent is temporarily unavailable but can not be interviewed for our fieldwork period" 5 "Respondent completely refused to be interviewed" 6 "Respondent is mad (mentally challenged)/very sick and can not be interviewed"

ds *reason, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __reason = .
            replace __reason = 12 if trim(`v') == "12"
            replace __reason = 3 if trim(`v') == "3"
            replace __reason = 4 if trim(`v') == "4"
            replace __reason = 5 if trim(`v') == "5"
            replace __reason = 6 if trim(`v') == "6"
            drop `v'
            rename __reason `v'
            label values `v' vl_reason
            label variable `v' "Reason for failing to contact/find farmer"
    }
}

ds *q6, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "We visited this household between September and November last year and our records indicate we talked to you, ${farmer_name}. Is this correct? Did we talk to you?"
}
ds *q6x1, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Can I talk to the person we talked to between September and November last year?"
}
ds *q9, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace 
    label values `v' yesno
    label variable `v' "Hello, my name is... Can we proceed with the interview?"
}
cap label drop vl_hh_educ
label define vl_hh_educ 1 "No formal education" 2 "Some primary" 3 "Finished primary" 4 "Some secondary" 5 "Finished secondary" 6 "Higher than secondary" 96 "Other" 8 "Don’t know"
ds *hh_educ, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __hh_educ = .
            replace __hh_educ = 1 if trim(`v') == "1"
            replace __hh_educ = 2 if trim(`v') == "2"
            replace __hh_educ = 3 if trim(`v') == "3"
            replace __hh_educ = 4 if trim(`v') == "4"
            replace __hh_educ = 5 if trim(`v') == "5"
            replace __hh_educ = 6 if trim(`v') == "6"
            replace __hh_educ = 96 if trim(`v') == "96"
            replace __hh_educ = 98 if trim(`v') == "98"
            drop `v'
            rename __hh_educ `v'
            label values `v' vl_hh_educ
            label variable `v' "Q3. What is the education level of the household head?"
    }
}
cap label drop vl_hh_gender
label define vl_hh_gender 1 "Male" 2 "Female"
ds *hh_gender, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __hh_gender = .
            replace __hh_gender = 1 if trim(`v') == "Male"
            replace __hh_gender = 2 if trim(`v') == "Female"
            drop `v'
            rename __hh_gender `v'
            label values `v' vl_hh_gender
            label variable `v' "Q4. What is the gender of the household head?"
    }
}
ds *aip_rec, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q7. Did the household of the respondent benefit from AIP in the most recent (2024/2025) agricultural season?"
}
ds *aip_rec_t_1, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q8. Did the household of the respondent benefit from AIP season preceding the most recent season (that is the 2023/2024 season)?"
}
cap label drop vl_feed_diff
label define vl_feed_diff 1 "Never" 2 "Seldom" 3 "Sometimes" 4 "Often" 5 "Very often, nearly always"
ds *feed_diff, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __feed_diff = .
            replace __feed_diff = 1 if trim(`v') == "1"
            replace __feed_diff = 2 if trim(`v') == "2"
            replace __feed_diff = 3 if trim(`v') == "3"
            replace __feed_diff = 4 if trim(`v') == "4"
            replace __feed_diff = 5 if trim(`v') == "5"
            drop `v'
            rename __feed_diff `v'
            label values `v' vl_feed_diff
            label variable `v' "Q9. Did you have difficulties in feeding your family last year (that is in 2024)?"
    }
}
ds *is_fw, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q11. Is this nearest agro-input shop, a Farmers World agro-input shop"
}
ds *rem_test, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q13. Do you remember that when we visited you between September and November 2024, we took some soil samples?"
}
ds *get_rec, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q14. Did you get the fertilizer recommendations based on this soil test on your plot? It would come in the form of a sheet similar to this:"
}
cap label drop vl_ease_rec
label define vl_ease_rec 1 "Very easy to understand" 2 "Easy to understand" 3 "Neutral" 4 "Hard to understand" 5 "I did not understand anything of this"
ds *ease_rec, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __ease_rec = .
            replace __ease_rec = 1 if trim(`v') == "1"
            replace __ease_rec = 2 if trim(`v') == "2"
            replace __ease_rec = 3 if trim(`v') == "3"
            replace __ease_rec = 4 if trim(`v') == "4"
            replace __ease_rec = 5 if trim(`v') == "5"
            drop `v'
            rename __ease_rec `v'
            label values `v' vl_ease_rec
            label variable `v' "Q15. How easy were the recommendations to understand?"
    }
}
ds *folll_rec, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q15b.  Did you follow the recommendations?"
}
ds *got_voucher, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q16. Did you get a voucher to buy fertilizer in a Farmers World Shop?"
}
ds *buy_rec, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q17. Did you buy the fertilizer that was recommended in the soil test report?"
}
ds *redeem, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q18. Did you go and redeem the voucher at the designated Farmers World shop?"
}
ds *reco_av, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q19. Was the recommended fertilizer available?"
}
ds *buy_other, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q20. Did you get another type of fertilizer instead of the one that was recommended?"
}
ds *vou_en, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q21. Was the voucher value enough to buy all the recommended fertilizer?"
}
ds *agro_vis, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q22. Did an Agronaut  (Farmers World Extension worker)  visit you to provide further extension services on the crops you grew in the 2024/2025 growing season??"
}
ds *ext_srv, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q24. Did you get any extension services from other extension providers on the crops you grew in the 2024/25 season?"
}
cap label drop vl_other_ext
label define vl_other_ext 1 "Government extension worker" 2 "NGO extension worker" 96 "Other" 4 "Don’t know"
ds *other_ext, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __other_ext = .
            replace __other_ext = 1 if trim(`v') == "1"
            replace __other_ext = 2 if trim(`v') == "2"
            replace __other_ext = 96 if trim(`v') == "96"
            replace __other_ext = 98 if trim(`v') == "98"
            drop `v'
            rename __other_ext `v'
            label values `v' vl_other_ext
            label variable `v' "Q25. From whom did you get other extension services  in the 2024/25 season?"
    }
}
cap label drop vl_slope
label define vl_slope 1 "Flat" 2 "Gentle slope" 3 "Moderate slope" 4 "Steep" 98 "Dont Know"
ds *slope, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __slope = .
            replace __slope = 1 if trim(`v') == "1"
            replace __slope = 2 if trim(`v') == "2"
            replace __slope = 3 if trim(`v') == "3"
            replace __slope = 4 if trim(`v') == "4"
            replace __slope = 98 if trim(`v') == "98"
            drop `v'
            rename __slope `v'
            label values `v' vl_slope
            label variable `v' "Q29. What is the `v' of this ${plot_samp} plot that we picked the soil sample from?"
    }
}
cap label drop vl_soil_str
label define vl_soil_str 1 "Sandy" 2 "Clay" 3 "Loam" 4 "Silty" 98 "Don't know"
ds *soil_str, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __soil_str = .
            replace __soil_str = 1 if trim(`v') == "1"
            replace __soil_str = 2 if trim(`v') == "2"
            replace __soil_str = 3 if trim(`v') == "3"
            replace __soil_str = 4 if trim(`v') == "4"
            replace __soil_str = 5 if trim(`v') == "88"
            drop `v'
            rename __soil_str `v'
            label values `v' vl_soil_str
            label variable `v' "Q93. Describe the soil type/texture of this ${plot_select_name} plot."
    }
}
cap label drop vl_main_crp
label define vl_main_crp 1 "MAIZE" 2 "BEANS" 3 "SOYABEAN" 4 "GROUNDNUTS" 5 "TOBACCO" 6 "SWEET_POTATO" 7 "IRISH_POTATO" 8 "RICE" 9 "BANANA" 10 "PEACH" 11 "PEA" 12 "MASAU" 13 "MILLET" 14 "PAWPAW" 15 "PIGEONPEA_NANDOLO" 16 "SORGHUM" 17 "SUNFLOWER" 18 "SUGARCANE" 19 "CABBAGE" 20 "NKHWANI" 21 "THERERE/OKRA" 22 "TOMATO" 23 "ONION" 24 "CASSAVA" 25 "DONT_KNOW"
ds *main_crp, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __main_crp = .
            replace __main_crp = 1 if trim(`v') == "MAIZE"
            replace __main_crp = 2 if trim(`v') == "BEANS"
            replace __main_crp = 3 if trim(`v') == "SOYABEAN"
            replace __main_crp = 4 if trim(`v') == "GROUNDNUTS"
            replace __main_crp = 5 if trim(`v') == "TOBACCO"
            replace __main_crp = 6 if trim(`v') == "SWEET_POTATO"
            replace __main_crp = 7 if trim(`v') == "IRISH_POTATO"
            replace __main_crp = 8 if trim(`v') == "RICE"
            replace __main_crp = 9 if trim(`v') == "BANANA"
            replace __main_crp = 10 if trim(`v') == "PEACH"
            replace __main_crp = 11 if trim(`v') == "PEA"
            replace __main_crp = 12 if trim(`v') == "MASAU"
            replace __main_crp = 13 if trim(`v') == "MILLET"
            replace __main_crp = 14 if trim(`v') == "PAWPAW"
            replace __main_crp = 15 if trim(`v') == "PIGEONPEA_NANDOLO"
            replace __main_crp = 16 if trim(`v') == "SORGHUM"
            replace __main_crp = 17 if trim(`v') == "SUNFLOWER"
            replace __main_crp = 18 if trim(`v') == "SUGARCANE"
            replace __main_crp = 19 if trim(`v') == "CABBAGE"
            replace __main_crp = 20 if trim(`v') == "NKHWANI"
            replace __main_crp = 21 if trim(`v') == "THERERE/OKRA"
            replace __main_crp = 22 if trim(`v') == "TOMATO"
            replace __main_crp = 23 if trim(`v') == "ONION"
            replace __main_crp = 24 if trim(`v') == "CASSAVA"
            replace __main_crp = 25 if trim(`v') == "DONT_KNOW"
            drop `v'
            rename __main_crp `v'
            label values `v' vl_main_crp
            label variable `v' "Q32. What was the **main** crop you planted on this ${plot_samp} plot during the most recent agricultural season (2024/25)?"
    }
}
ds *int_crpp, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q33. Was this ${plot_samp} plot inter-cropped with other crops during the most recent agricultural season (2024/25)?"
}
cap label drop vl_crp_cropp
label define vl_crp_cropp 1 "MAIZE" 2 "BEANS" 3 "SOYABEAN" 4 "GROUNDNUTS" 5 "TOBACCO" 6 "SWEET_POTATO" 7 "IRISH_POTATO" 8 "RICE" 9 "BANANA" 10 "PEACH" 11 "PEA" 12 "MASAU" 13 "MILLET" 14 "PAWPAW" 15 "PIGEONPEA_NANDOLO" 16 "SORGHUM" 17 "SUNFLOWER" 18 "SUGARCANE" 19 "CABBAGE" 20 "NKHWANI" 21 "THERERE/OKRA" 22 "TOMATO" 23 "ONION" 24 "CASSAVA" 25 "DONT_KNOW"
ds *crp_cropp, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __crp_cropp = .
            replace __crp_cropp = 1 if trim(`v') == "MAIZE"
            replace __crp_cropp = 2 if trim(`v') == "BEANS"
            replace __crp_cropp = 3 if trim(`v') == "SOYABEAN"
            replace __crp_cropp = 4 if trim(`v') == "GROUNDNUTS"
            replace __crp_cropp = 5 if trim(`v') == "TOBACCO"
            replace __crp_cropp = 6 if trim(`v') == "SWEET_POTATO"
            replace __crp_cropp = 7 if trim(`v') == "IRISH_POTATO"
            replace __crp_cropp = 8 if trim(`v') == "RICE"
            replace __crp_cropp = 9 if trim(`v') == "BANANA"
            replace __crp_cropp = 10 if trim(`v') == "PEACH"
            replace __crp_cropp = 11 if trim(`v') == "PEA"
            replace __crp_cropp = 12 if trim(`v') == "MASAU"
            replace __crp_cropp = 13 if trim(`v') == "MILLET"
            replace __crp_cropp = 14 if trim(`v') == "PAWPAW"
            replace __crp_cropp = 15 if trim(`v') == "PIGEONPEA_NANDOLO"
            replace __crp_cropp = 16 if trim(`v') == "SORGHUM"
            replace __crp_cropp = 17 if trim(`v') == "SUNFLOWER"
            replace __crp_cropp = 18 if trim(`v') == "SUGARCANE"
            replace __crp_cropp = 19 if trim(`v') == "CABBAGE"
            replace __crp_cropp = 20 if trim(`v') == "NKHWANI"
            replace __crp_cropp = 21 if trim(`v') == "THERERE/OKRA"
            replace __crp_cropp = 22 if trim(`v') == "TOMATO"
            replace __crp_cropp = 23 if trim(`v') == "ONION"
            replace __crp_cropp = 24 if trim(`v') == "CASSAVA"
            replace __crp_cropp = 25 if trim(`v') == "DONT_KNOW"
            drop `v'
            rename __crp_cropp `v'
            label values `v' vl_crp_cropp
            label variable `v' "Q34. What was the crop that was inter-cropped with during the most recent agricultural season (2024/25)? Select multiple if needed."
    }
}
cap label drop vl_seed_typ
label define vl_seed_typ 1 "Indigenous/Local variety" 2 "Improved & newly acquired last season" 3 "Improved but reused/recycled" 4 "I don't know"
ds *seed_typ, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __seed_typ = .
            replace __seed_typ = 1 if trim(`v') == "Indigenous_Local variety"
            replace __seed_typ = 2 if trim(`v') == "Improved_newly acquired last season"
            replace __seed_typ = 3 if trim(`v') == "Improved_ reused more than once"
            replace __seed_typ = 4 if trim(`v') == "unknown"
            drop `v'
            rename __seed_typ `v'
            label values `v' vl_seed_typ
            label variable `v' "Q36. What was the type of seed or planting material used on this ${plot_samp} for the main crop during the most recent agricultural season (2024/25)?"
    }
}
ds *seed_bag, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q37. Was the seed acquired in a sealed bag with a tag, label, or code?"
}
cap label drop vl_seed_var
label define vl_seed_var 1 "LOCA" 2 "MH29" 3 "MH53" 4 "MH28" 5 "MH27" 6 "MH26" 7 "MH15" 8 "MH16" 9 "NSCM41" 10 "MH18" 11 "MH17" 12 "PAN 6479" 13 "PAN 53" 14 "PAN 6777" 15 "PAN 33" 16 "PAN 4M-21" 17 "MH20" 18 "MH43A" 19 "MH21" 20 "SENSAKO2151" 21 "MH122" 22 "NSCM51" 23 "DK8089 (Fumba)" 24 "DKC 8033 (Mapasa)" 25 "DKC8643/7500 (Maziko)" 26 "DKC7500  (Maziko)" 27 "DKC 8053 (Nkangala)" 28 "DK 8051" 29 "DK 8031" 30 "DK8041" 31 "PHB 30D79" 32 "PHB 30V33" 33 "ZM 521" 34 "ZM 421" 35 "SC 301 (Kalulu)" 36 "SC 303 (Kalulu)" 37 "SC 665 (Mkango)" 38 "SC 515 (Kanyani)" 39 "SC 501 (Kanyani)" 40 "SC 407 (Kanyani)" 41 "SC 555 (Mbidzi)" 42 "SC 559 (Kanyani)" 43 "SC 729 (Njovu)" 44 "SC 719 (Njovu)" 45 "ZM621" 46 "Unknown" 96 "Other" 48 "LOCAL" 49 "NUA 45" 50 "NUA 59" 51 "BC-D/O(19)" 52 "KABALABALA" 53 "KALIMA" 54 "KHOLOPHETH" 55 "MKHALIRA" 56 "NAPILIRA" 57 "SAPATSIKA" 58 "MALUWA" 59 "NAGAGA" 60 "KAMBIDZI" 61 "Unknown" 96 "Other" 63 "LOCAL" 64 "PAN 1868" 65 "TIKOLORE" 66 "MAKWACHA" 67 "NASOKO" 68 "DAVIS" 69 "BOSSIER" 70 "Unknown" 96 "Other" 72 "LOCAL" 73 "CHALIMBANA" 74 "CHETEMBANA" 75 "CG8" 76 "RG1" 77 "MANI-PINTAR" 78 "MAWANGA ." 79 "JL24" 80 "BAKA" 81 "CHITALA" 82 "Unknown" 96 "Other" 84 "BURLEY" 85 "FLUE CURED" 86 "NNDF" 87 "SDF" 88 "RIENTAL" 89 "Unknown" 96 "Other" 91 "IRISH_POTATO" 92 "LOCAL" 93 "FAYA" 94 "PUSA" 95 "TCG10" 96 "IET4094 (SENGA)" 97 "WAMBONE" 98 "KILOMBERO" 99 "ITA" 100 "MTUPATUPA" 101 "Unknown" 96 "Other" 103 "LOCAL" 104 "WHITE" 105 "YELLOW" 106 "NJULI" 107 "BEMBEKE" 108 "ZIKOMO" 109 "THANDIZO" 110 "VIOLET" 111 "CHUMA" 112 "MWAI" 113 "ROSITA" 114 "ROSLIN TSANGANO" 115 "ROSLIN BVUMBWE" 96 "Other" 117 "LOCAL" 118 "BANANA" 119 "PEACH" 120 "PEA" 121 "MASAU" 122 "PAWPAW" 123 "SUNFLOWER" 124 "SUGARCANE" 125 "CABBAGE" 126 "NKHWANI" 127 "THERERE/OKRA" 128 "TOMATO" 129 "ONION" 130 "CASSAVA" 131 "unknown" 96 "Other" 133 "TEA" 134 "SORGHUM" 135 "MILLET" 136 "PIGEONPEA_NANDOLO" 137 "SWEET_POTATO"
ds *seed_var, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __seed_var = .
            replace __seed_var = 1 if trim(`v') == "LOCA"
            replace __seed_var = 2 if trim(`v') == "MH29"
            replace __seed_var = 3 if trim(`v') == "MH53"
            replace __seed_var = 4 if trim(`v') == "MH28"
            replace __seed_var = 5 if trim(`v') == "MH27"
            replace __seed_var = 6 if trim(`v') == "MH26"
            replace __seed_var = 7 if trim(`v') == "MH15"
            replace __seed_var = 8 if trim(`v') == "MH16"
            replace __seed_var = 9 if trim(`v') == "NSCM41"
            replace __seed_var = 10 if trim(`v') == "MH18"
            replace __seed_var = 11 if trim(`v') == "MH17"
            replace __seed_var = 12 if trim(`v') == "PAN 6479"
            replace __seed_var = 13 if trim(`v') == "PAN 53"
            replace __seed_var = 14 if trim(`v') == "PAN 6777"
            replace __seed_var = 15 if trim(`v') == "PAN 33"
            replace __seed_var = 16 if trim(`v') == "PAN 4M-21"
            replace __seed_var = 17 if trim(`v') == "MH20"
            replace __seed_var = 18 if trim(`v') == "MH43A"
            replace __seed_var = 19 if trim(`v') == "MH21"
            replace __seed_var = 20 if trim(`v') == "SENSAKO2151"
            replace __seed_var = 21 if trim(`v') == "MH122"
            replace __seed_var = 22 if trim(`v') == "NSCM51"
            replace __seed_var = 23 if trim(`v') == "DK8089"
            replace __seed_var = 24 if trim(`v') == "DKC 8033"
            replace __seed_var = 25 if trim(`v') == "DKC8643"
            replace __seed_var = 26 if trim(`v') == "DKC7500"
            replace __seed_var = 27 if trim(`v') == "DKC 8053"
            replace __seed_var = 28 if trim(`v') == "DK 8051"
            replace __seed_var = 29 if trim(`v') == "DK 8031"
            replace __seed_var = 30 if trim(`v') == "DK8041"
            replace __seed_var = 31 if trim(`v') == "PHB 30D79"
            replace __seed_var = 32 if trim(`v') == "PHB 30V33"
            replace __seed_var = 33 if trim(`v') == "ZM 521"
            replace __seed_var = 34 if trim(`v') == "ZM 421"
            replace __seed_var = 35 if trim(`v') == "SC 301"
            replace __seed_var = 36 if trim(`v') == "SC 303"
            replace __seed_var = 37 if trim(`v') == "SC 665"
            replace __seed_var = 38 if trim(`v') == "SC 515"
            replace __seed_var = 39 if trim(`v') == "SC 501"
            replace __seed_var = 40 if trim(`v') == "SC 407"
            replace __seed_var = 41 if trim(`v') == "SC 555"
            replace __seed_var = 42 if trim(`v') == "SC 559"
            replace __seed_var = 43 if trim(`v') == "SC 729"
            replace __seed_var = 44 if trim(`v') == "SC 719"
            replace __seed_var = 45 if trim(`v') == "ZM621"
            replace __seed_var = 46 if trim(`v') == "Unknown"
            replace __seed_var = 47 if trim(`v') == "Other"
            replace __seed_var = 48 if trim(`v') == "LOCAL"
            replace __seed_var = 49 if trim(`v') == "NUA 45"
            replace __seed_var = 50 if trim(`v') == "NUA 59"
            replace __seed_var = 51 if trim(`v') == "BC-D/O(19)"
            replace __seed_var = 52 if trim(`v') == "KABALABALA"
            replace __seed_var = 53 if trim(`v') == "KALIMA"
            replace __seed_var = 54 if trim(`v') == "KHOLOPHETH"
            replace __seed_var = 55 if trim(`v') == "MKHALIRA"
            replace __seed_var = 56 if trim(`v') == "NAPILIRA"
            replace __seed_var = 57 if trim(`v') == "SAPATSIKA"
            replace __seed_var = 58 if trim(`v') == "MALUWA"
            replace __seed_var = 59 if trim(`v') == "NAGAGA"
            replace __seed_var = 60 if trim(`v') == "KAMBIDZI"
            replace __seed_var = 61 if trim(`v') == "Unknown"
            replace __seed_var = 62 if trim(`v') == "Other"
            replace __seed_var = 63 if trim(`v') == "LOCAL"
            replace __seed_var = 64 if trim(`v') == "PAN 1867"
            replace __seed_var = 65 if trim(`v') == "TIKOLORE"
            replace __seed_var = 66 if trim(`v') == "MAKWACHA"
            replace __seed_var = 67 if trim(`v') == "NASOKO"
            replace __seed_var = 68 if trim(`v') == "DAVIS"
            replace __seed_var = 69 if trim(`v') == "BOSSIER"
            replace __seed_var = 70 if trim(`v') == "Unknown"
            replace __seed_var = 71 if trim(`v') == "Other"
            replace __seed_var = 72 if trim(`v') == "LOCAL"
            replace __seed_var = 73 if trim(`v') == "CHALIMBANA"
            replace __seed_var = 74 if trim(`v') == "CHETEMBANA"
            replace __seed_var = 75 if trim(`v') == "CG7"
            replace __seed_var = 76 if trim(`v') == "RG1"
            replace __seed_var = 77 if trim(`v') == "MANI-PINTAR"
            replace __seed_var = 78 if trim(`v') == "MAWANGA ."
            replace __seed_var = 79 if trim(`v') == "JL24"
            replace __seed_var = 80 if trim(`v') == "BAKA"
            replace __seed_var = 81 if trim(`v') == "CHITALA"
            replace __seed_var = 82 if trim(`v') == "Unknown"
            replace __seed_var = 83 if trim(`v') == "Other"
            replace __seed_var = 84 if trim(`v') == "BURLEY"
            replace __seed_var = 85 if trim(`v') == "FLUE CURED"
            replace __seed_var = 86 if trim(`v') == "NNDF"
            replace __seed_var = 87 if trim(`v') == "SDF"
            replace __seed_var = 88 if trim(`v') == "RIENTAL"
            replace __seed_var = 89 if trim(`v') == "Unknown"
            replace __seed_var = 90 if trim(`v') == "Other"
            replace __seed_var = 91 if trim(`v') == "IRISH_POTATO"
            replace __seed_var = 92 if trim(`v') == "LOCAL"
            replace __seed_var = 93 if trim(`v') == "FAYA"
            replace __seed_var = 94 if trim(`v') == "PUSA"
            replace __seed_var = 95 if trim(`v') == "TCG10"
            replace __seed_var = 96 if trim(`v') == "IET4094 (SENGA)"
            replace __seed_var = 97 if trim(`v') == "WAMBONE"
            replace __seed_var = 98 if trim(`v') == "KILOMBERO"
            replace __seed_var = 99 if trim(`v') == "ITA"
            replace __seed_var = 100 if trim(`v') == "MTUPATUPA"
            replace __seed_var = 101 if trim(`v') == "Unknown"
            replace __seed_var = 102 if trim(`v') == "Other"
            replace __seed_var = 103 if trim(`v') == "LOCAL"
            replace __seed_var = 104 if trim(`v') == "WHITE"
            replace __seed_var = 105 if trim(`v') == "YELLOW"
            replace __seed_var = 106 if trim(`v') == "NJULI"
            replace __seed_var = 107 if trim(`v') == "BEMBEKE"
            replace __seed_var = 108 if trim(`v') == "ZIKOMO"
            replace __seed_var = 109 if trim(`v') == "THANDIZO"
            replace __seed_var = 110 if trim(`v') == "VIOLET"
            replace __seed_var = 111 if trim(`v') == "CHUMA"
            replace __seed_var = 112 if trim(`v') == "MWAI"
            replace __seed_var = 113 if trim(`v') == "ROSITA"
            replace __seed_var = 114 if trim(`v') == "ROSLIN TSANGANO"
            replace __seed_var = 115 if trim(`v') == "ROSLIN BVUMBWE"
            replace __seed_var = 116 if trim(`v') == "Other"
            replace __seed_var = 117 if trim(`v') == "LOCAL"
            replace __seed_var = 118 if trim(`v') == "BANANA"
            replace __seed_var = 119 if trim(`v') == "PEACH"
            replace __seed_var = 120 if trim(`v') == "PEA"
            replace __seed_var = 121 if trim(`v') == "MASAU"
            replace __seed_var = 122 if trim(`v') == "PAWPAW"
            replace __seed_var = 123 if trim(`v') == "SUNFLOWER"
            replace __seed_var = 124 if trim(`v') == "SUGARCANE"
            replace __seed_var = 125 if trim(`v') == "CABBAGE"
            replace __seed_var = 126 if trim(`v') == "NKHWANI"
            replace __seed_var = 127 if trim(`v') == "THERERE/OKRA"
            replace __seed_var = 128 if trim(`v') == "TOMATO"
            replace __seed_var = 129 if trim(`v') == "ONION"
            replace __seed_var = 130 if trim(`v') == "CASSAVA"
            replace __seed_var = 131 if trim(`v') == "unknown"
            replace __seed_var = 132 if trim(`v') == "Other"
            replace __seed_var = 133 if trim(`v') == "TEA"
            replace __seed_var = 134 if trim(`v') == "SORGHUM"
            replace __seed_var = 135 if trim(`v') == "MILLET"
            replace __seed_var = 136 if trim(`v') == "PIGEONPEA_NANDOLO"
            replace __seed_var = 137 if trim(`v') == "SWEET_POTATO"
            drop `v'
            rename __seed_var `v'
            label values `v' vl_seed_var
            label variable `v' "Q38. What is the name of the ${main_crp} variety seed or planting material used for the main crop on this ${plot_samp} during the most recent agricultural season (2024/25)?"
    }
}
cap label drop vl_seed_sourc
label define vl_seed_sourc 1 "Farmer seed multiplier" 2 "Agro dealers" 3 "NGO" 4 "Market" 5 "Agricultural cooperatives" 6 "Neighbor/another farmer" 7 "Credit groups" 8 "Church/religious groups" 9 "Own seed/Recycled" 96 "Other," 98 "Dont know"
ds *seed_sourc, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __seed_sourc = .
            replace __seed_sourc = 1 if trim(`v') == "1"
            replace __seed_sourc = 2 if trim(`v') == "2"
            replace __seed_sourc = 3 if trim(`v') == "3"
            replace __seed_sourc = 4 if trim(`v') == "4"
            replace __seed_sourc = 5 if trim(`v') == "5"
            replace __seed_sourc = 6 if trim(`v') == "6"
            replace __seed_sourc = 7 if trim(`v') == "7"
            replace __seed_sourc = 8 if trim(`v') == "8"
            replace __seed_sourc = 9 if trim(`v') == "9"
            replace __seed_sourc = 96 if trim(`v') == "96"
            replace __seed_sourc = 98 if trim(`v') == "98"
            drop `v'
            rename __seed_sourc `v'
            label values `v' vl_seed_sourc
            label variable `v' "Q39. From where did you obtain this ${seed_var} seed that you planted on this ${plot_samp} during the most recent agricultural season (2024/2025)?"
    }
}
ds *seed_ob_aip, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q43. During the most recent agricultural season (2024/25), did some of this ${seed_var} variety used on this ${plot_samp} plot come from purchases through the Affordable Inputs Program (AIP)?"
}
cap label drop vl_pay_meth
label define vl_pay_meth 1 "Free" 2 "Own cash/savings" 3 "Credit-Bank" 4 "Credit-Micro finance" 5 "Credit-friends/relatives" 6 "Labor exchange" 7 "A combination" 8 "Contract farming/agreement" 96 "Other" 98 "Dont know"
ds *pay_meth, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __pay_meth = .
            replace __pay_meth = 1 if trim(`v') == "1"
            replace __pay_meth = 2 if trim(`v') == "2"
            replace __pay_meth = 3 if trim(`v') == "3"
            replace __pay_meth = 4 if trim(`v') == "4"
            replace __pay_meth = 5 if trim(`v') == "5"
            replace __pay_meth = 6 if trim(`v') == "6"
            replace __pay_meth = 7 if trim(`v') == "7"
            replace __pay_meth = 8 if trim(`v') == "8"
            replace __pay_meth = 96 if trim(`v') == "96"
            replace __pay_meth = 98 if trim(`v') == "98"
            drop `v'
            rename __pay_meth `v'
            label values `v' vl_pay_meth
            label variable `v' "Q46. How did you pay for [most of] this ${qty_used} kg of ${seed_var} planted on this ${plot_samp} in the most recent season (2024/2025)?"
    }
}
cap label drop vl_satis_qual
label define vl_satis_qual 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_qual, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_qual = .
            replace __satis_qual = 1 if trim(`v') == "1"
            replace __satis_qual = 2 if trim(`v') == "2"
            replace __satis_qual = 3 if trim(`v') == "3"
            replace __satis_qual = 4 if trim(`v') == "4"
            replace __satis_qual = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_qual `v'
            label values `v' vl_satis_qual
            label variable `v' "Q47. How satisfied are you in terms of the overall QUALITY of the seed variety you planted?"
    }
}
cap label drop vl_satis_yield
label define vl_satis_yield 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_yield, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_yield = .
            replace __satis_yield = 1 if trim(`v') == "1"
            replace __satis_yield = 2 if trim(`v') == "2"
            replace __satis_yield = 3 if trim(`v') == "3"
            replace __satis_yield = 4 if trim(`v') == "4"
            replace __satis_yield = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_yield `v'
            label values `v' vl_satis_yield
            label variable `v' "Q49. Overall, how satisfied are you with the **yield** you obtained using the seed variety you planted?"
    }
}
cap label drop vl_satis_dis
label define vl_satis_dis 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_dis, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_dis = .
            replace __satis_dis = 1 if trim(`v') == "1"
            replace __satis_dis = 2 if trim(`v') == "2"
            replace __satis_dis = 3 if trim(`v') == "3"
            replace __satis_dis = 4 if trim(`v') == "4"
            replace __satis_dis = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_dis `v'
            label values `v' vl_satis_dis
            label variable `v' "Q50. Overall, how satisfied are you in terms of the **disease resistance** of the seed variety you planted?"
    }
}
cap label drop vl_satis_drought
label define vl_satis_drought 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_drought, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_drought = .
            replace __satis_drought = 1 if trim(`v') == "1"
            replace __satis_drought = 2 if trim(`v') == "2"
            replace __satis_drought = 3 if trim(`v') == "3"
            replace __satis_drought = 4 if trim(`v') == "4"
            replace __satis_drought = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_drought `v'
            label values `v' vl_satis_drought
            label variable `v' "Q51. Overall, how satisfied are you in terms of the **drought tolerance** of the seed variety you planted?"
    }
}
cap label drop vl_satis_avail
label define vl_satis_avail 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_avail, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_avail = .
            replace __satis_avail = 1 if trim(`v') == "1"
            replace __satis_avail = 2 if trim(`v') == "2"
            replace __satis_avail = 3 if trim(`v') == "3"
            replace __satis_avail = 4 if trim(`v') == "4"
            replace __satis_avail = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_avail `v'
            label values `v' vl_satis_avail
            label variable `v' "Q52. Overall, how satisfied are you in terms of the overall **availability and access** of good quality seeds?"
    }
}
cap label drop vl_plant_time
label define vl_plant_time 1 "Before the rains started" 2 "Immediately after the first rains (one to 3 days)" 3 "About one week after the first rains" 4 "More than one week after first rains" 98 "Don't know"
ds *plant_time, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __plant_time = .
            replace __plant_time = 1 if trim(`v') == "1"
            replace __plant_time = 2 if trim(`v') == "2"
            replace __plant_time = 3 if trim(`v') == "3"
            replace __plant_time = 4 if trim(`v') == "4"
            replace __plant_time = 5 if trim(`v') == "88"
            drop `v'
            rename __plant_time `v'
            label values `v' vl_plant_time
            label variable `v' "Q54. When did you plant the seed on this ${plot_samp} plot in the most recent agricultural season of 2024/2025?"
    }
}
cap label drop vl_seed_rate
label define vl_seed_rate 1 "1 seed per planting station" 2 "2 seeds per planting station" 3 "3 seeds per planting station" 4 "More than 3 seeds per planting station" 98 "Dont Know"
ds *seed_rate, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __seed_rate = .
            replace __seed_rate = 1 if trim(`v') == "1"
            replace __seed_rate = 2 if trim(`v') == "2"
            replace __seed_rate = 3 if trim(`v') == "3"
            replace __seed_rate = 4 if trim(`v') == "4"
            replace __seed_rate = 98 if trim(`v') == "98"
            drop `v'
            rename __seed_rate `v'
            label values `v' vl_seed_rate
            label variable `v' "Q55. What was the seed rate/plant rate on this ${plot_samp} plot in the most recent agricultural season of 2024/2025?"
    }
}
cap label drop vl_fert_time
label define vl_fert_time 1 "During land preparation" 2 "During planting" 3 "During growth (basal dressing)" 4 "During growth (top dressing)" 5 "don’t know"
ds *fert_time, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __fert_time = .
            replace __fert_time = 1 if trim(`v') == "1"
            replace __fert_time = 2 if trim(`v') == "2"
            replace __fert_time = 3 if trim(`v') == "3"
            replace __fert_time = 4 if trim(`v') == "4"
            replace __fert_time = 98 if trim(`v') == "98"
            drop `v'
            rename __fert_time `v'
            label values `v' vl_fert_time
            label variable `v' "Q57. When was this ${fertilizer_name} applied on this ${plot_samp} plot in the most recent agricultural season of 2024/2025?"
    }
}
cap label drop vl_fert_app_meth
label define vl_fert_app_meth 1 "Dollop method In hill under seed" 2 "Broadcasting" 3 "Spraying" 98 "Dont know"
ds *fert_app_meth, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __fert_app_meth = .
            replace __fert_app_meth = 1 if trim(`v') == "1"
            replace __fert_app_meth = 2 if trim(`v') == "2"
            replace __fert_app_meth = 3 if trim(`v') == "3"
            replace __fert_app_meth = 98 if trim(`v') == "98"
            drop `v'
            rename __fert_app_meth `v'
            label values `v' vl_fert_app_meth
            label variable `v' "Q58. How was this ${fertilizer_name} applied on this ${plot_samp} plot in the most recent agricultural season of 2024/2025?"
    }
}
cap label drop vl_weather
label define vl_weather 1 "Sunny" 2 "Overcasting" 3 "Showers" 4 "Rain" 5 "Dont remember"
ds *weather, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __weather = .
            replace __weather = 1 if trim(`v') == "1"
            replace __weather = 2 if trim(`v') == "2"
            replace __weather = 3 if trim(`v') == "3"
            replace __weather = 4 if trim(`v') == "4"
            replace __weather = 5 if trim(`v') == "99"
            drop `v'
            rename __weather `v'
            label values `v' vl_weather
            label variable `v' "Q59. What was the `v' like duing the ${fertilizer_name} application on this ${plot_samp} plot in the most recent agricultural season of 2024/2025?"
    }
}
ds *ob_aip, has(type string)

if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen byte __tmp = .
        replace __tmp = 1 if lower(trim(`v')) == "yes"
        replace __tmp = 0 if lower(trim(`v')) == "no"

        drop `v'
        rename __tmp `v'

        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno

        label variable `v' ///
            "Q62. Was some or all of this ${fertilizer_name} applied on this ${plot_samp} plot in the most recent agricultural season of 2024/2025 part of the fertilizer obtained through AIP?"
    }
}
else {
    di as txt "No *ob_aip variables found that are string."
}
ds *fert_us, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q65. Was part of this ${fertilizer_name} applied on this ${plot_samp} plot in the most recent agricultural season of 2024/2025 obtained through a voucher you received from us (similar to this one)?"
}
ds *fert_sat, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q69. Where you satisfied with the ${fertilizer_name} used on this ${plot_samp} plot in the most recent agricultural season of 2024/2025?"
}
cap label drop vl_weed_time
label define vl_weed_time 1 "Did not weed at all" 2 "Once" 3 "Twice" 4 "3 or more than 3 times" 98 "Dont know"
ds *weed_time, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __weed_time = .
            replace __weed_time = 1 if trim(`v') == "1"
            replace __weed_time = 2 if trim(`v') == "2"
            replace __weed_time = 3 if trim(`v') == "3"
            replace __weed_time = 4 if trim(`v') == "4"
            replace __weed_time = 98 if trim(`v') == "98"
            drop `v'
            rename __weed_time `v'
            label values `v' vl_weed_time
            label variable `v' "Q70. How many times did you weed this ${plot_samp} plot during the most recent agricultural season of 2024/2025?"
    }
}
ds *pest_use, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q71. Did you use any pesticides, herbicides or fungicides on ${plot_samp} plot during the most recent agricultural season of 2024/2025?"
}
ds *green_inco, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q73. Did you engage in early incorporation of green legume manure crop (Beans, soya beans, cow peas, ground nuts etc) before the most recent agricultural  season (2024/2025) on this ${plot_samp} plot?"
}
cap label drop vl_crp_prec
label define vl_crp_prec 1 "MAIZE" 2 "BEANS" 3 "SOYABEAN" 4 "GROUNDNUTS" 5 "TOBACCO" 6 "SWEET_POTATO" 7 "IRISH_POTATO" 8 "RICE" 9 "BANANA" 10 "PEACH" 11 "PEA" 12 "MASAU" 13 "MILLET" 14 "PAWPAW" 15 "PIGEONPEA_NANDOLO" 16 "SORGHUM" 17 "SUNFLOWER" 18 "SUGARCANE" 19 "CABBAGE" 20 "NKHWANI" 21 "THERERE/OKRA" 22 "TOMATO" 23 "ONION" 24 "CASSAVA" 25 "DONT_KNOW"
ds *crp_prec, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __crp_prec = .
            replace __crp_prec = 1 if trim(`v') == "MAIZE"
            replace __crp_prec = 2 if trim(`v') == "BEANS"
            replace __crp_prec = 3 if trim(`v') == "SOYABEAN"
            replace __crp_prec = 4 if trim(`v') == "GROUNDNUTS"
            replace __crp_prec = 5 if trim(`v') == "TOBACCO"
            replace __crp_prec = 6 if trim(`v') == "SWEET_POTATO"
            replace __crp_prec = 7 if trim(`v') == "IRISH_POTATO"
            replace __crp_prec = 8 if trim(`v') == "RICE"
            replace __crp_prec = 9 if trim(`v') == "BANANA"
            replace __crp_prec = 10 if trim(`v') == "PEACH"
            replace __crp_prec = 11 if trim(`v') == "PEA"
            replace __crp_prec = 12 if trim(`v') == "MASAU"
            replace __crp_prec = 13 if trim(`v') == "MILLET"
            replace __crp_prec = 14 if trim(`v') == "PAWPAW"
            replace __crp_prec = 15 if trim(`v') == "PIGEONPEA_NANDOLO"
            replace __crp_prec = 16 if trim(`v') == "SORGHUM"
            replace __crp_prec = 17 if trim(`v') == "SUNFLOWER"
            replace __crp_prec = 18 if trim(`v') == "SUGARCANE"
            replace __crp_prec = 19 if trim(`v') == "CABBAGE"
            replace __crp_prec = 20 if trim(`v') == "NKHWANI"
            replace __crp_prec = 21 if trim(`v') == "THERERE/OKRA"
            replace __crp_prec = 22 if trim(`v') == "TOMATO"
            replace __crp_prec = 23 if trim(`v') == "ONION"
            replace __crp_prec = 24 if trim(`v') == "CASSAVA"
            replace __crp_prec = 25 if trim(`v') == "DONT_KNOW"
            drop `v'
            rename __crp_prec `v'
            label values `v' vl_crp_prec
            label variable `v' "Q74. What crop was grown on this ${plot_samp} plot in the season preceding the most recent agricultural season (that is the 2023/2024 season)?"
    }
}
ds *fresh_app, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q75. Did you apply fresh vegetative material (mulching) before the most recent agricultural  season (2024/2025) on this ${plot_samp} plot?"
}
ds *mat_farm_app, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q76. Did you apply dry material farmyard manure before the most recent agricultural season (2024/2025) on this ${plot_samp} plot"
}
ds *dairy_app, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q77. Did you apply dry material dairy or poultry manure before the most recent agricultural season (2024/2025) on this ${plot_samp} plot?"
}
ds *comp_app, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q78. Did you apply compost before the most recent agricultural season 2024/2025 on this ${plot_samp} plot?"
}
ds *mbeya_app, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q79. Did you apply mbeya fertilizer before the most recent agricultural season 2024/2025 on this ${plot_samp} plot?"
}
ds *till_app, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q80. Did you apply minimum tillage on this ${plot_samp} plot during the most recent agricultural season (2024/2025)?"
}
ds *ridge_use, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q81. Did you use ridges, check dams, swallies or other soil erosion prevention techniques on this ${plot_samp} plot during the most recent agricultural season (2024/2025)?"
}
ds *pit_use, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q81b. Did you use pit planting or  techniques on this ${plot_samp} plot during the most recent agricultural season (2024/2025)?"
}
ds *main_sell, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q83. Did you sell any of the ${main_crp} that you harvested on this ${plot_samp} during the most recent agricultural season (2024/2025)?"
}
ds *other_exp, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q87b. Did you have any other  expenses such as labour, irrIgation expenses, etc for this ${plot_samp} plot  in the recent agricultural  season of 2024/2025?"
}
ds *other_plots, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q88. In addition to the plot where the soil sample was taken, did you cultivate other plots in the 2024/2025 season?"
}
cap label drop vl_sloper
label define vl_sloper 1 "Flat" 2 "Gentle slope" 3 "Moderate slope" 4 "Steep" 98 "Dont Know"
ds *sloper, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __sloper = .
            replace __sloper = 1 if trim(`v') == "1"
            replace __sloper = 2 if trim(`v') == "2"
            replace __sloper = 3 if trim(`v') == "3"
            replace __sloper = 4 if trim(`v') == "4"
            replace __sloper = 98 if trim(`v') == "98"
            drop `v'
            rename __sloper `v'
            label values `v' vl_sloper
            label variable `v' "Q92. What is the slope of this ${plot_select_name} plot that we picked the soil sample from?"
    }
}
cap label drop vl_main_crpr
label define vl_main_crpr 1 "MAIZE" 2 "BEANS" 3 "SOYABEAN" 4 "GROUNDNUTS" 5 "TOBACCO" 6 "SWEET_POTATO" 7 "IRISH_POTATO" 8 "RICE" 9 "BANANA" 10 "PEACH" 11 "PEA" 12 "MASAU" 13 "MILLET" 14 "PAWPAW" 15 "PIGEONPEA_NANDOLO" 16 "SORGHUM" 17 "SUNFLOWER" 18 "SUGARCANE" 19 "CABBAGE" 20 "NKHWANI" 21 "THERERE/OKRA" 22 "TOMATO" 23 "ONION" 24 "CASSAVA" 25 "DONT_KNOW"
ds *main_crpr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __main_crpr = .
            replace __main_crpr = 1 if trim(`v') == "MAIZE"
            replace __main_crpr = 2 if trim(`v') == "BEANS"
            replace __main_crpr = 3 if trim(`v') == "SOYABEAN"
            replace __main_crpr = 4 if trim(`v') == "GROUNDNUTS"
            replace __main_crpr = 5 if trim(`v') == "TOBACCO"
            replace __main_crpr = 6 if trim(`v') == "SWEET_POTATO"
            replace __main_crpr = 7 if trim(`v') == "IRISH_POTATO"
            replace __main_crpr = 8 if trim(`v') == "RICE"
            replace __main_crpr = 9 if trim(`v') == "BANANA"
            replace __main_crpr = 10 if trim(`v') == "PEACH"
            replace __main_crpr = 11 if trim(`v') == "PEA"
            replace __main_crpr = 12 if trim(`v') == "MASAU"
            replace __main_crpr = 13 if trim(`v') == "MILLET"
            replace __main_crpr = 14 if trim(`v') == "PAWPAW"
            replace __main_crpr = 15 if trim(`v') == "PIGEONPEA_NANDOLO"
            replace __main_crpr = 16 if trim(`v') == "SORGHUM"
            replace __main_crpr = 17 if trim(`v') == "SUNFLOWER"
            replace __main_crpr = 18 if trim(`v') == "SUGARCANE"
            replace __main_crpr = 19 if trim(`v') == "CABBAGE"
            replace __main_crpr = 20 if trim(`v') == "NKHWANI"
            replace __main_crpr = 21 if trim(`v') == "THERERE/OKRA"
            replace __main_crpr = 22 if trim(`v') == "TOMATO"
            replace __main_crpr = 23 if trim(`v') == "ONION"
            replace __main_crpr = 24 if trim(`v') == "CASSAVA"
            replace __main_crpr = 25 if trim(`v') == "DONT_KNOW"
            drop `v'
            rename __main_crpr `v'
            label values `v' vl_main_crpr
            label variable `v' "Q95. What was the main crop you planted on this ${plot_select_name} plot during the most recent agricultural season (2024/25)?"
    }
}
ds *int_crppr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q96. Was this ${plot_select_name} plot inter-cropped with other crops during the most recent agricultural season (2024/25)?"
}
cap label drop vl_crp_croppr
label define vl_crp_croppr 1 "MAIZE" 2 "BEANS" 3 "SOYABEAN" 4 "GROUNDNUTS" 5 "TOBACCO" 6 "SWEET_POTATO" 7 "IRISH_POTATO" 8 "RICE" 9 "BANANA" 10 "PEACH" 11 "PEA" 12 "MASAU" 13 "MILLET" 14 "PAWPAW" 15 "PIGEONPEA_NANDOLO" 16 "SORGHUM" 17 "SUNFLOWER" 18 "SUGARCANE" 19 "CABBAGE" 20 "NKHWANI" 21 "THERERE/OKRA" 22 "TOMATO" 23 "ONION" 24 "CASSAVA" 25 "DONT_KNOW"
ds *crp_croppr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __crp_croppr = .
            replace __crp_croppr = 1 if trim(`v') == "MAIZE"
            replace __crp_croppr = 2 if trim(`v') == "BEANS"
            replace __crp_croppr = 3 if trim(`v') == "SOYABEAN"
            replace __crp_croppr = 4 if trim(`v') == "GROUNDNUTS"
            replace __crp_croppr = 5 if trim(`v') == "TOBACCO"
            replace __crp_croppr = 6 if trim(`v') == "SWEET_POTATO"
            replace __crp_croppr = 7 if trim(`v') == "IRISH_POTATO"
            replace __crp_croppr = 8 if trim(`v') == "RICE"
            replace __crp_croppr = 9 if trim(`v') == "BANANA"
            replace __crp_croppr = 10 if trim(`v') == "PEACH"
            replace __crp_croppr = 11 if trim(`v') == "PEA"
            replace __crp_croppr = 12 if trim(`v') == "MASAU"
            replace __crp_croppr = 13 if trim(`v') == "MILLET"
            replace __crp_croppr = 14 if trim(`v') == "PAWPAW"
            replace __crp_croppr = 15 if trim(`v') == "PIGEONPEA_NANDOLO"
            replace __crp_croppr = 16 if trim(`v') == "SORGHUM"
            replace __crp_croppr = 17 if trim(`v') == "SUNFLOWER"
            replace __crp_croppr = 18 if trim(`v') == "SUGARCANE"
            replace __crp_croppr = 19 if trim(`v') == "CABBAGE"
            replace __crp_croppr = 20 if trim(`v') == "NKHWANI"
            replace __crp_croppr = 21 if trim(`v') == "THERERE/OKRA"
            replace __crp_croppr = 22 if trim(`v') == "TOMATO"
            replace __crp_croppr = 23 if trim(`v') == "ONION"
            replace __crp_croppr = 24 if trim(`v') == "CASSAVA"
            replace __crp_croppr = 25 if trim(`v') == "DONT_KNOW"
            drop `v'
            rename __crp_croppr `v'
            label values `v' vl_crp_croppr
            label variable `v' "Q97. What was the crop that was inter-cropped with during the most recent agricultural season (2024/25)? Select multiple if needed."
    }
}
cap label drop vl_seed_typr
label define vl_seed_typr 1 "Indigenous/Local variety" 2 "Improved & newly acquired last season" 3 "Improved but reused/recycled" 4 "I don't know"
ds *seed_typr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __seed_typr = .
            replace __seed_typr = 1 if trim(`v') == "Indigenous_Local variety"
            replace __seed_typr = 2 if trim(`v') == "Improved_newly acquired last season"
            replace __seed_typr = 3 if trim(`v') == "Improved_ reused more than once"
            replace __seed_typr = 4 if trim(`v') == "unknown"
            drop `v'
            rename __seed_typr `v'
            label values `v' vl_seed_typr
            label variable `v' "Q99. What was type of seed or planting material used on this ${plot_select_name} for the main crop during the most recent agricultural season (2024/25)?"
    }
}
ds *seed_bagr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q100. Was the seed acquired in a sealed bag with a tag, label, or code?"
}
cap label drop vl_seed_varr
label define vl_seed_varr 1 "LOCA" 2 "MH29" 3 "MH53" 4 "MH28" 5 "MH27" 6 "MH26" 7 "MH15" 8 "MH16" 9 "NSCM41" 10 "MH18" 11 "MH17" 12 "PAN 6479" 13 "PAN 53" 14 "PAN 6777" 15 "PAN 33" 16 "PAN 4M-21" 17 "MH20" 18 "MH43A" 19 "MH21" 20 "SENSAKO2151" 21 "MH122" 22 "NSCM51" 23 "DK8089 (Fumba)" 24 "DKC 8033 (Mapasa)" 25 "DKC8643/7500 (Maziko)" 26 "DKC7500  (Maziko)" 27 "DKC 8053 (Nkangala)" 28 "DK 8051" 29 "DK 8031" 30 "DK8041" 31 "PHB 30D79" 32 "PHB 30V33" 33 "ZM 521" 34 "ZM 421" 35 "SC 301 (Kalulu)" 36 "SC 303 (Kalulu)" 37 "SC 665 (Mkango)" 38 "SC 515 (Kanyani)" 39 "SC 501 (Kanyani)" 40 "SC 407 (Kanyani)" 41 "SC 555 (Mbidzi)" 42 "SC 559 (Kanyani)" 43 "SC 729 (Njovu)" 44 "SC 719 (Njovu)" 45 "ZM621" 46 "Unknown" 96 "Other" 48 "LOCAL" 49 "NUA 45" 50 "NUA 59" 51 "BC-D/O(19)" 52 "KABALABALA" 53 "KALIMA" 54 "KHOLOPHETH" 55 "MKHALIRA" 56 "NAPILIRA" 57 "SAPATSIKA" 58 "MALUWA" 59 "NAGAGA" 60 "KAMBIDZI" 61 "Unknown" 96 "Other" 63 "LOCAL" 64 "PAN 1868" 65 "TIKOLORE" 66 "MAKWACHA" 67 "NASOKO" 68 "DAVIS" 69 "BOSSIER" 70 "Unknown" 96 "Other" 72 "LOCAL" 73 "CHALIMBANA" 74 "CHETEMBANA" 75 "CG8" 76 "RG1" 77 "MANI-PINTAR" 78 "MAWANGA ." 79 "JL24" 80 "BAKA" 81 "CHITALA" 82 "Unknown" 96 "Other" 84 "BURLEY" 85 "FLUE CURED" 86 "NNDF" 87 "SDF" 88 "RIENTAL" 89 "Unknown" 96 "Other" 91 "IRISH_POTATO" 92 "LOCAL" 93 "FAYA" 94 "PUSA" 95 "TCG10" 96 "IET4094 (SENGA)" 97 "WAMBONE" 98 "KILOMBERO" 99 "ITA" 100 "MTUPATUPA" 101 "Unknown" 96 "Other" 103 "LOCAL" 104 "WHITE" 105 "YELLOW" 106 "NJULI" 107 "BEMBEKE" 108 "ZIKOMO" 109 "THANDIZO" 110 "VIOLET" 111 "CHUMA" 112 "MWAI" 113 "ROSITA" 114 "ROSLIN TSANGANO" 115 "ROSLIN BVUMBWE" 96 "Other" 117 "LOCAL" 118 "BANANA" 119 "PEACH" 120 "PEA" 121 "MASAU" 122 "PAWPAW" 123 "SUNFLOWER" 124 "SUGARCANE" 125 "CABBAGE" 126 "NKHWANI" 127 "THERERE/OKRA" 128 "TOMATO" 129 "ONION" 130 "CASSAVA" 131 "unknown" 96 "Other" 133 "TEA" 134 "SORGHUM" 135 "MILLET" 136 "PIGEONPEA_NANDOLO" 137 "SWEET_POTATO"
ds *seed_varr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __seed_varr = .
            replace __seed_varr = 1 if trim(`v') == "LOCA"
            replace __seed_varr = 2 if trim(`v') == "MH29"
            replace __seed_varr = 3 if trim(`v') == "MH53"
            replace __seed_varr = 4 if trim(`v') == "MH28"
            replace __seed_varr = 5 if trim(`v') == "MH27"
            replace __seed_varr = 6 if trim(`v') == "MH26"
            replace __seed_varr = 7 if trim(`v') == "MH15"
            replace __seed_varr = 8 if trim(`v') == "MH16"
            replace __seed_varr = 9 if trim(`v') == "NSCM41"
            replace __seed_varr = 10 if trim(`v') == "MH18"
            replace __seed_varr = 11 if trim(`v') == "MH17"
            replace __seed_varr = 12 if trim(`v') == "PAN 6479"
            replace __seed_varr = 13 if trim(`v') == "PAN 53"
            replace __seed_varr = 14 if trim(`v') == "PAN 6777"
            replace __seed_varr = 15 if trim(`v') == "PAN 33"
            replace __seed_varr = 16 if trim(`v') == "PAN 4M-21"
            replace __seed_varr = 17 if trim(`v') == "MH20"
            replace __seed_varr = 18 if trim(`v') == "MH43A"
            replace __seed_varr = 19 if trim(`v') == "MH21"
            replace __seed_varr = 20 if trim(`v') == "SENSAKO2151"
            replace __seed_varr = 21 if trim(`v') == "MH122"
            replace __seed_varr = 22 if trim(`v') == "NSCM51"
            replace __seed_varr = 23 if trim(`v') == "DK8089"
            replace __seed_varr = 24 if trim(`v') == "DKC 8033"
            replace __seed_varr = 25 if trim(`v') == "DKC8643"
            replace __seed_varr = 26 if trim(`v') == "DKC7500"
            replace __seed_varr = 27 if trim(`v') == "DKC 8053"
            replace __seed_varr = 28 if trim(`v') == "DK 8051"
            replace __seed_varr = 29 if trim(`v') == "DK 8031"
            replace __seed_varr = 30 if trim(`v') == "DK8041"
            replace __seed_varr = 31 if trim(`v') == "PHB 30D79"
            replace __seed_varr = 32 if trim(`v') == "PHB 30V33"
            replace __seed_varr = 33 if trim(`v') == "ZM 521"
            replace __seed_varr = 34 if trim(`v') == "ZM 421"
            replace __seed_varr = 35 if trim(`v') == "SC 301"
            replace __seed_varr = 36 if trim(`v') == "SC 303"
            replace __seed_varr = 37 if trim(`v') == "SC 665"
            replace __seed_varr = 38 if trim(`v') == "SC 515"
            replace __seed_varr = 39 if trim(`v') == "SC 501"
            replace __seed_varr = 40 if trim(`v') == "SC 407"
            replace __seed_varr = 41 if trim(`v') == "SC 555"
            replace __seed_varr = 42 if trim(`v') == "SC 559"
            replace __seed_varr = 43 if trim(`v') == "SC 729"
            replace __seed_varr = 44 if trim(`v') == "SC 719"
            replace __seed_varr = 45 if trim(`v') == "ZM621"
            replace __seed_varr = 46 if trim(`v') == "Unknown"
            replace __seed_varr = 47 if trim(`v') == "Other"
            replace __seed_varr = 48 if trim(`v') == "LOCAL"
            replace __seed_varr = 49 if trim(`v') == "NUA 45"
            replace __seed_varr = 50 if trim(`v') == "NUA 59"
            replace __seed_varr = 51 if trim(`v') == "BC-D/O(19)"
            replace __seed_varr = 52 if trim(`v') == "KABALABALA"
            replace __seed_varr = 53 if trim(`v') == "KALIMA"
            replace __seed_varr = 54 if trim(`v') == "KHOLOPHETH"
            replace __seed_varr = 55 if trim(`v') == "MKHALIRA"
            replace __seed_varr = 56 if trim(`v') == "NAPILIRA"
            replace __seed_varr = 57 if trim(`v') == "SAPATSIKA"
            replace __seed_varr = 58 if trim(`v') == "MALUWA"
            replace __seed_varr = 59 if trim(`v') == "NAGAGA"
            replace __seed_varr = 60 if trim(`v') == "KAMBIDZI"
            replace __seed_varr = 61 if trim(`v') == "Unknown"
            replace __seed_varr = 62 if trim(`v') == "Other"
            replace __seed_varr = 63 if trim(`v') == "LOCAL"
            replace __seed_varr = 64 if trim(`v') == "PAN 1867"
            replace __seed_varr = 65 if trim(`v') == "TIKOLORE"
            replace __seed_varr = 66 if trim(`v') == "MAKWACHA"
            replace __seed_varr = 67 if trim(`v') == "NASOKO"
            replace __seed_varr = 68 if trim(`v') == "DAVIS"
            replace __seed_varr = 69 if trim(`v') == "BOSSIER"
            replace __seed_varr = 70 if trim(`v') == "Unknown"
            replace __seed_varr = 71 if trim(`v') == "Other"
            replace __seed_varr = 72 if trim(`v') == "LOCAL"
            replace __seed_varr = 73 if trim(`v') == "CHALIMBANA"
            replace __seed_varr = 74 if trim(`v') == "CHETEMBANA"
            replace __seed_varr = 75 if trim(`v') == "CG7"
            replace __seed_varr = 76 if trim(`v') == "RG1"
            replace __seed_varr = 77 if trim(`v') == "MANI-PINTAR"
            replace __seed_varr = 78 if trim(`v') == "MAWANGA ."
            replace __seed_varr = 79 if trim(`v') == "JL24"
            replace __seed_varr = 80 if trim(`v') == "BAKA"
            replace __seed_varr = 81 if trim(`v') == "CHITALA"
            replace __seed_varr = 82 if trim(`v') == "Unknown"
            replace __seed_varr = 83 if trim(`v') == "Other"
            replace __seed_varr = 84 if trim(`v') == "BURLEY"
            replace __seed_varr = 85 if trim(`v') == "FLUE CURED"
            replace __seed_varr = 86 if trim(`v') == "NNDF"
            replace __seed_varr = 87 if trim(`v') == "SDF"
            replace __seed_varr = 88 if trim(`v') == "RIENTAL"
            replace __seed_varr = 89 if trim(`v') == "Unknown"
            replace __seed_varr = 90 if trim(`v') == "Other"
            replace __seed_varr = 91 if trim(`v') == "IRISH_POTATO"
            replace __seed_varr = 92 if trim(`v') == "LOCAL"
            replace __seed_varr = 93 if trim(`v') == "FAYA"
            replace __seed_varr = 94 if trim(`v') == "PUSA"
            replace __seed_varr = 95 if trim(`v') == "TCG10"
            replace __seed_varr = 96 if trim(`v') == "IET4094 (SENGA)"
            replace __seed_varr = 97 if trim(`v') == "WAMBONE"
            replace __seed_varr = 98 if trim(`v') == "KILOMBERO"
            replace __seed_varr = 99 if trim(`v') == "ITA"
            replace __seed_varr = 100 if trim(`v') == "MTUPATUPA"
            replace __seed_varr = 101 if trim(`v') == "Unknown"
            replace __seed_varr = 102 if trim(`v') == "Other"
            replace __seed_varr = 103 if trim(`v') == "LOCAL"
            replace __seed_varr = 104 if trim(`v') == "WHITE"
            replace __seed_varr = 105 if trim(`v') == "YELLOW"
            replace __seed_varr = 106 if trim(`v') == "NJULI"
            replace __seed_varr = 107 if trim(`v') == "BEMBEKE"
            replace __seed_varr = 108 if trim(`v') == "ZIKOMO"
            replace __seed_varr = 109 if trim(`v') == "THANDIZO"
            replace __seed_varr = 110 if trim(`v') == "VIOLET"
            replace __seed_varr = 111 if trim(`v') == "CHUMA"
            replace __seed_varr = 112 if trim(`v') == "MWAI"
            replace __seed_varr = 113 if trim(`v') == "ROSITA"
            replace __seed_varr = 114 if trim(`v') == "ROSLIN TSANGANO"
            replace __seed_varr = 115 if trim(`v') == "ROSLIN BVUMBWE"
            replace __seed_varr = 116 if trim(`v') == "Other"
            replace __seed_varr = 117 if trim(`v') == "LOCAL"
            replace __seed_varr = 118 if trim(`v') == "BANANA"
            replace __seed_varr = 119 if trim(`v') == "PEACH"
            replace __seed_varr = 120 if trim(`v') == "PEA"
            replace __seed_varr = 121 if trim(`v') == "MASAU"
            replace __seed_varr = 122 if trim(`v') == "PAWPAW"
            replace __seed_varr = 123 if trim(`v') == "SUNFLOWER"
            replace __seed_varr = 124 if trim(`v') == "SUGARCANE"
            replace __seed_varr = 125 if trim(`v') == "CABBAGE"
            replace __seed_varr = 126 if trim(`v') == "NKHWANI"
            replace __seed_varr = 127 if trim(`v') == "THERERE/OKRA"
            replace __seed_varr = 128 if trim(`v') == "TOMATO"
            replace __seed_varr = 129 if trim(`v') == "ONION"
            replace __seed_varr = 130 if trim(`v') == "CASSAVA"
            replace __seed_varr = 131 if trim(`v') == "unknown"
            replace __seed_varr = 132 if trim(`v') == "Other"
            replace __seed_varr = 133 if trim(`v') == "TEA"
            replace __seed_varr = 134 if trim(`v') == "SORGHUM"
            replace __seed_varr = 135 if trim(`v') == "MILLET"
            replace __seed_varr = 136 if trim(`v') == "PIGEONPEA_NANDOLO"
            replace __seed_varr = 137 if trim(`v') == "SWEET_POTATO"
            drop `v'
            rename __seed_varr `v'
            label values `v' vl_seed_varr
            label variable `v' "Q101. What is the name of the ${main_crpr} variety seed or planting material used for the main crop on this ${plot_select_name} during the most recent agricultural season (2024/25)?"
    }
}
cap label drop vl_seed_sourcr
label define vl_seed_sourcr 1 "Farmer seed multiplier" 2 "Agro dealers" 3 "NGO" 4 "Market" 5 "Agricultural cooperatives" 6 "Neighbor/another farmer" 7 "Credit groups" 8 "Church/religious groups" 9 "Own seed/Recycled" 96 "Other," 98 "Dont know"
ds *seed_sourcr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __seed_sourcr = .
            replace __seed_sourcr = 1 if trim(`v') == "1"
            replace __seed_sourcr = 2 if trim(`v') == "2"
            replace __seed_sourcr = 3 if trim(`v') == "3"
            replace __seed_sourcr = 4 if trim(`v') == "4"
            replace __seed_sourcr = 5 if trim(`v') == "5"
            replace __seed_sourcr = 6 if trim(`v') == "6"
            replace __seed_sourcr = 7 if trim(`v') == "7"
            replace __seed_sourcr = 8 if trim(`v') == "8"
            replace __seed_sourcr = 9 if trim(`v') == "9"
            replace __seed_sourcr = 96 if trim(`v') == "96"
            replace __seed_sourcr = 98 if trim(`v') == "98"
            drop `v'
            rename __seed_sourcr `v'
            label values `v' vl_seed_sourcr
            label variable `v' "Q102. From where did you obtain this ${seed_varr} that you planted on this ${plot_select_name} during the most recent agricultural season (2024/2025)?"
    }
}
ds *seed_ob_aipr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q106. During the most recent agricultural season (2024/25), did some of this ${seed_varr} variety used on this ${plot_select_name} plot come from purchases through the Affordable Inputs Program (AIP)?"
}
cap label drop vl_pay_methr
label define vl_pay_methr 1 "Free" 2 "Own cash/savings" 3 "Credit-Bank" 4 "Credit-Micro finance" 5 "Credit-friends/relatives" 6 "Labor exchange" 7 "A combination" 8 "Contract farming/agreement" 96 "Other" 98 "Dont know"
ds *pay_methr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __pay_methr = .
            replace __pay_methr = 1 if trim(`v') == "1"
            replace __pay_methr = 2 if trim(`v') == "2"
            replace __pay_methr = 3 if trim(`v') == "3"
            replace __pay_methr = 4 if trim(`v') == "4"
            replace __pay_methr = 5 if trim(`v') == "5"
            replace __pay_methr = 6 if trim(`v') == "6"
            replace __pay_methr = 7 if trim(`v') == "7"
            replace __pay_methr = 8 if trim(`v') == "8"
            replace __pay_methr = 96 if trim(`v') == "96"
            replace __pay_methr = 98 if trim(`v') == "98"
            drop `v'
            rename __pay_methr `v'
            label values `v' vl_pay_methr
            label variable `v' "Q109. How did you pay for [most of] this ${qty_usedr} Kg of ${seed_varr} planted on this ${plot_select_name} in the most recent seaon (2024/2025)?"
    }
}
cap label drop vl_satis_qualr
label define vl_satis_qualr 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_qualr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_qualr = .
            replace __satis_qualr = 1 if trim(`v') == "1"
            replace __satis_qualr = 2 if trim(`v') == "2"
            replace __satis_qualr = 3 if trim(`v') == "3"
            replace __satis_qualr = 4 if trim(`v') == "4"
            replace __satis_qualr = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_qualr `v'
            label values `v' vl_satis_qualr
            label variable `v' "Q110. How satisfied are you in terms of the overall QUALITY of the seed variety you planted?"
    }
}
cap label drop vl_satis_yieldr
label define vl_satis_yieldr 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_yieldr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_yieldr = .
            replace __satis_yieldr = 1 if trim(`v') == "1"
            replace __satis_yieldr = 2 if trim(`v') == "2"
            replace __satis_yieldr = 3 if trim(`v') == "3"
            replace __satis_yieldr = 4 if trim(`v') == "4"
            replace __satis_yieldr = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_yieldr `v'
            label values `v' vl_satis_yieldr
            label variable `v' "Q112. Overall, how satisfied are you with the **yield** you obtained using the seed variety you planted?"
    }
}
cap label drop vl_satis_disr
label define vl_satis_disr 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_disr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_disr = .
            replace __satis_disr = 1 if trim(`v') == "1"
            replace __satis_disr = 2 if trim(`v') == "2"
            replace __satis_disr = 3 if trim(`v') == "3"
            replace __satis_disr = 4 if trim(`v') == "4"
            replace __satis_disr = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_disr `v'
            label values `v' vl_satis_disr
            label variable `v' "Q113. Overall, how satisfied are you in terms of the **disease resistance** of the seed variety you planted?"
    }
}
cap label drop vl_satis_droughtr
label define vl_satis_droughtr 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_droughtr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_droughtr = .
            replace __satis_droughtr = 1 if trim(`v') == "1"
            replace __satis_droughtr = 2 if trim(`v') == "2"
            replace __satis_droughtr = 3 if trim(`v') == "3"
            replace __satis_droughtr = 4 if trim(`v') == "4"
            replace __satis_droughtr = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_droughtr `v'
            label values `v' vl_satis_droughtr
            label variable `v' "Q114. Overall, how satisfied are you in terms of the **drought tolerance** of the seed variety you planted?"
    }
}
cap label drop vl_satis_availr
label define vl_satis_availr 1 "Not satisfied" 2 "Slightly satisfied" 3 "Satisﬁed" 4 "Very Satisfied" 5 "Extremely satisfied"
ds *satis_availr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __satis_availr = .
            replace __satis_availr = 1 if trim(`v') == "1"
            replace __satis_availr = 2 if trim(`v') == "2"
            replace __satis_availr = 3 if trim(`v') == "3"
            replace __satis_availr = 4 if trim(`v') == "4"
            replace __satis_availr = 5 if trim(`v') == "5"
            drop `v'
            rename __satis_availr `v'
            label values `v' vl_satis_availr
            label variable `v' "Q115. Overall, how satisfied are you in terms of the overall **availability and access** of good quality seeds?"
    }
}
cap label drop vl_plant_timer
label define vl_plant_timer 1 "Before the rains started" 2 "Immediately after the first rains (one to 3 days)" 3 "About one week after the first rains" 4 "More than one week after first rains" 98 "Don't know"
ds *plant_timer, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __plant_timer = .
            replace __plant_timer = 1 if trim(`v') == "1"
            replace __plant_timer = 2 if trim(`v') == "2"
            replace __plant_timer = 3 if trim(`v') == "3"
            replace __plant_timer = 4 if trim(`v') == "4"
            replace __plant_timer = 5 if trim(`v') == "88"
            drop `v'
            rename __plant_timer `v'
            label values `v' vl_plant_timer
            label variable `v' "Q117. When did you plant the seed on this ${plot_select_name} plot in the most recent agricultural season of 2024/2025?"
    }
}
cap label drop vl_seed_rater
label define vl_seed_rater 1 "1 seed per planting station" 2 "2 seeds per planting station" 3 "3 seeds per planting station" 4 "More than 3 seeds per planting station" 98 "Dont Know"
ds *seed_rater, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __seed_rater = .
            replace __seed_rater = 1 if trim(`v') == "1"
            replace __seed_rater = 2 if trim(`v') == "2"
            replace __seed_rater = 3 if trim(`v') == "3"
            replace __seed_rater = 4 if trim(`v') == "4"
            replace __seed_rater = 98 if trim(`v') == "98"
            drop `v'
            rename __seed_rater `v'
            label values `v' vl_seed_rater
            label variable `v' "Q118. What was the seed rate/plant rate on this ${plot_select_name} plot in the most recent agricultural season of 2024/2025?"
    }
}
cap label drop vl_fert_timer
label define vl_fert_timer 1 "During land preparation" 2 "During planting" 3 "During growth (basal dressing)" 4 "During growth (top dressing)" 5 "don’t know"
ds *fert_timer, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __fert_timer = .
            replace __fert_timer = 1 if trim(`v') == "1"
            replace __fert_timer = 2 if trim(`v') == "2"
            replace __fert_timer = 3 if trim(`v') == "3"
            replace __fert_timer = 4 if trim(`v') == "4"
            replace __fert_timer = 98 if trim(`v') == "98"
            drop `v'
            rename __fert_timer `v'
            label values `v' vl_fert_timer
            label variable `v' "Q120. When was this ${fertilizer_namer} applied on this ${plot_select_name} plot in the most recent agricultural season of 2024/2025?"
    }
}
cap label drop vl_fert_app_methr
label define vl_fert_app_methr 1 "Dollop method In hill under seed" 2 "Broadcasting" 3 "Spraying" 98 "Dont know"
ds *fert_app_methr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __fert_app_methr = .
            replace __fert_app_methr = 1 if trim(`v') == "1"
            replace __fert_app_methr = 2 if trim(`v') == "2"
            replace __fert_app_methr = 3 if trim(`v') == "3"
            replace __fert_app_methr = 98 if trim(`v') == "98"
            drop `v'
            rename __fert_app_methr `v'
            label values `v' vl_fert_app_methr
            label variable `v' "Q121. How was this ${fertilizer_namer} applied on this ${plot_select_name} plot in the most recent agricultural season of 2024/2025?"
    }
}
cap label drop vl_weatherr
label define vl_weatherr 1 "Sunny" 2 "Overcasting" 3 "Showers" 4 "Rain" 5 "Dont remember"
ds *weatherr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __weatherr = .
            replace __weatherr = 1 if trim(`v') == "1"
            replace __weatherr = 2 if trim(`v') == "2"
            replace __weatherr = 3 if trim(`v') == "3"
            replace __weatherr = 4 if trim(`v') == "4"
            replace __weatherr = 5 if trim(`v') == "99"
            drop `v'
            rename __weatherr `v'
            label values `v' vl_weatherr
            label variable `v' "Q122. What was the weather like duing the ${fertilizer_namer} application on this ${plot_select_name} plot in the most recent agricultural season of 2024/2025?"
    }
}
ds *ob_aipr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q125. Was some or all of this ${fertilizer_namer} applied on this ${plot_select_name} plot in the most recent agricultural season of 2024/2025 part of the fertilizer obtained through AIP?"
}
ds *fert_usr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q128. Was part of this ${fertilizer_namer} applied on this ${plot_select_name} plot in the most recent agricultural season of 2024/2025 obtained through a voucher you received from us (similar to this one)?"
}
ds *fert_satr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q132. Where you satisfied with the ${fertilizer_namer} used on this ${plot_select_name} plot in the most recent agricultural season of 2024/2025?"
}
cap label drop vl_weed_timer
label define vl_weed_timer 1 "Did not weed at all" 2 "Once" 3 "Twice" 4 "3 or more than 3 times" 98 "Dont know"
ds *weed_timer, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __weed_timer = .
            replace __weed_timer = 1 if trim(`v') == "1"
            replace __weed_timer = 2 if trim(`v') == "2"
            replace __weed_timer = 3 if trim(`v') == "3"
            replace __weed_timer = 4 if trim(`v') == "4"
            replace __weed_timer = 98 if trim(`v') == "98"
            drop `v'
            rename __weed_timer `v'
            label values `v' vl_weed_timer
            label variable `v' "Q133. How many times did you weed this ${plot_select_name} plot during the most recent agricultural season of 2024/2025?"
    }
}
ds *pest_user, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q134. Did you use any pesticides, herbicides or fungicides on ${plot_select_name} plot during the most recent agricultural season of 2024/2025?"
}
ds *green_incor, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q136. Did you engage in early incorporation of green legume manure crop (Beans, soya beans, cow peas, ground nuts etc) before the most recent agriculutural season (2024/2025) on this ${plot_select_name} plot?"
}
cap label drop vl_crp_precr
label define vl_crp_precr 1 "MAIZE" 2 "BEANS" 3 "SOYABEAN" 4 "GROUNDNUTS" 5 "TOBACCO" 6 "SWEET_POTATO" 7 "IRISH_POTATO" 8 "RICE" 9 "BANANA" 10 "PEACH" 11 "PEA" 12 "MASAU" 13 "MILLET" 14 "PAWPAW" 15 "PIGEONPEA_NANDOLO" 16 "SORGHUM" 17 "SUNFLOWER" 18 "SUGARCANE" 19 "CABBAGE" 20 "NKHWANI" 21 "THERERE/OKRA" 22 "TOMATO" 23 "ONION" 24 "CASSAVA" 25 "DONT_KNOW"
ds *crp_precr, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __crp_precr = .
            replace __crp_precr = 1 if trim(`v') == "MAIZE"
            replace __crp_precr = 2 if trim(`v') == "BEANS"
            replace __crp_precr = 3 if trim(`v') == "SOYABEAN"
            replace __crp_precr = 4 if trim(`v') == "GROUNDNUTS"
            replace __crp_precr = 5 if trim(`v') == "TOBACCO"
            replace __crp_precr = 6 if trim(`v') == "SWEET_POTATO"
            replace __crp_precr = 7 if trim(`v') == "IRISH_POTATO"
            replace __crp_precr = 8 if trim(`v') == "RICE"
            replace __crp_precr = 9 if trim(`v') == "BANANA"
            replace __crp_precr = 10 if trim(`v') == "PEACH"
            replace __crp_precr = 11 if trim(`v') == "PEA"
            replace __crp_precr = 12 if trim(`v') == "MASAU"
            replace __crp_precr = 13 if trim(`v') == "MILLET"
            replace __crp_precr = 14 if trim(`v') == "PAWPAW"
            replace __crp_precr = 15 if trim(`v') == "PIGEONPEA_NANDOLO"
            replace __crp_precr = 16 if trim(`v') == "SORGHUM"
            replace __crp_precr = 17 if trim(`v') == "SUNFLOWER"
            replace __crp_precr = 18 if trim(`v') == "SUGARCANE"
            replace __crp_precr = 19 if trim(`v') == "CABBAGE"
            replace __crp_precr = 20 if trim(`v') == "NKHWANI"
            replace __crp_precr = 21 if trim(`v') == "THERERE/OKRA"
            replace __crp_precr = 22 if trim(`v') == "TOMATO"
            replace __crp_precr = 23 if trim(`v') == "ONION"
            replace __crp_precr = 24 if trim(`v') == "CASSAVA"
            replace __crp_precr = 25 if trim(`v') == "DONT_KNOW"
            drop `v'
            rename __crp_precr `v'
            label values `v' vl_crp_precr
            label variable `v' "Q137. What crop was grown on this ${plot_select_name} plot in the season preceding the most recent agricultural season (that is the 2023/2024 season)?"
    }
}
ds *fresh_appr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q138. Did you apply fresh vegetative material (mulching) before the most recent agriculutral season (2024/2025) on this ${plot_select_name} plot?"
}
ds *mat_farm_appr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q139. Did you apply dry material farmyard manure before the most recent agricultural season (2024/2025) on this ${plot_select_name} plot"
}
ds *dairy_appr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q40. Did you apply dry material dairy or poultry manure before the most recent agricultural season (2024/2025) on this ${plot_select_name} plot?"
}
ds *comp_appr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q141. Did you apply compost before the most recent agricultural season 2024/2025 on this ${plot_select_name} plot?"
}
ds *mbeya_appr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q142. Did you apply mbeya before the most recent agricultural season 2024/2025 on this ${plot_samp} plot?"
}
ds *till_appr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q143. Did you apply minimum tillage on this ${plot_select_name} plot during the most recent agricultural season (2024/2025)?"
}
ds *ridge_user, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q144. Did you use ridges, check dams, swallies or other soil erosion prevention techniques on this ${plot_select_name} plot during the most recent agricultural season (2024/2025)?"
}
ds *pit_user, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q81b. Did you use pit planting or  techniques on this ${plot_select_name} plot during the most recent agricultural season (2024/2025)?"
}
ds *main_sellr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q147. Did you sell any of the ${main_crpr} that you harvested on this ${plot_select_name} during the most recent agricultural season (2024/2025)?"
}
ds *other_expr, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q149b. Did you have any other  expenses such as labour, irrigation expenses, etc  for ${plot_select_name}  in  agricultural season (2024/2025)?"
}
ds *crop_failure, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q150. During the current agricultural season of 2024/2025, did you encounter crop failure due to any reason?"
}
cap label drop vl_reason_fail
label define vl_reason_fail 1 "Pests and Diseases" 2 "Weather and Climate Factors" 3 "Poor seed quality" 4 "Soil fertility" 5 "Poor agriculutural practices" 6 "Lack of inputs and resources" 7 "Labour shortage" 96 "Other"
ds *reason_fail, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __reason_fail = .
            replace __reason_fail = 1 if trim(`v') == "1"
            replace __reason_fail = 2 if trim(`v') == "2"
            replace __reason_fail = 3 if trim(`v') == "3"
            replace __reason_fail = 4 if trim(`v') == "4"
            replace __reason_fail = 5 if trim(`v') == "5"
            replace __reason_fail = 6 if trim(`v') == "6"
            replace __reason_fail = 7 if trim(`v') == "7"
            replace __reason_fail = 96 if trim(`v') == "96"
            drop `v'
            rename __reason_fail `v'
            label values `v' vl_reason_fail
            label variable `v' "Q152. What was the most important reason for the crop failure?"
    }
}
ds *imp_gnuts, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q153. Have you ever used improved groundnut seed?"
}
cap label drop vl_reason_gnuts
label define vl_reason_gnuts 1 "IMPROVED SEED WAS TOO EXPENSIVE/NOT PROFITABLE" 2 "PLOT/SOIL NOT SUITABLE FOR IMPROVED VARIETY" 3 "IMPROVED SEED YIELD IS LOW" 4 "FEAR OF LOSS OF HARVEST/DROUGHT/WATER SHORTAGE" 5 "FEAR OF LOSS OF HARVEST//DISEASE" 6 "QUALITY NOT TRUSTED/FEAR OF FAKE SEED" 7 "UNABLE TO ACCESS IMPROVED VARIETY I WANTED" 8 "LACK OF CASH/LOANS TO PAY UPFRONT" 9 "LACK OF ACCESS TO OTHER COMPLEMENTARY INPUTS SUCH AS FERTILIZERS" 10 "TASTE IS NOT PREFERRED OR COOKING QUALITY IS POOR" 11 "SEED IS NOT WEED RESISTANT" 96 "OTHER REASONS"
ds *reason_gnuts, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __reason_gnuts = .
            replace __reason_gnuts = 1 if trim(`v') == "1"
            replace __reason_gnuts = 2 if trim(`v') == "2"
            replace __reason_gnuts = 3 if trim(`v') == "3"
            replace __reason_gnuts = 4 if trim(`v') == "4"
            replace __reason_gnuts = 5 if trim(`v') == "5"
            replace __reason_gnuts = 6 if trim(`v') == "6"
            replace __reason_gnuts = 7 if trim(`v') == "7"
            replace __reason_gnuts = 8 if trim(`v') == "8"
            replace __reason_gnuts = 9 if trim(`v') == "9"
            replace __reason_gnuts = 10 if trim(`v') == "10"
            replace __reason_gnuts = 11 if trim(`v') == "11"
            replace __reason_gnuts = 96 if trim(`v') == "96"
            drop `v'
            rename __reason_gnuts `v'
            label values `v' vl_reason_gnuts
            label variable `v' "Q154. If NO to Question #20, what was the most important reason for not using improved groundnut seeds?"
    }
}
cap label drop vl_yield_fert
label define vl_yield_fert 1 "Short-term yield increases" 2 "Long-term soil fertility"
ds *yield_fert, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __yield_fert = .
            replace __yield_fert = 1 if trim(`v') == "1"
            replace __yield_fert = 2 if trim(`v') == "2"
            drop `v'
            rename __yield_fert `v'
            label values `v' vl_yield_fert
            label variable `v' "Q155. If you had to choose, would you prefer short-term yield increases or long-term soil fertility?"
    }
}
ds *org_amen, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q157. Would you choose organic soil amendments over chemical fertilizer if both were available at the same price?"
}
cap label drop vl_pay_hgh
label define vl_pay_hgh 1 "Nothing extra" 2 "Up to 10% more" 3 "Up to 25% more" 4 "More than 25%"
ds *pay_hgh, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __pay_hgh = .
            replace __pay_hgh = 1 if trim(`v') == "1"
            replace __pay_hgh = 2 if trim(`v') == "2"
            replace __pay_hgh = 3 if trim(`v') == "3"
            replace __pay_hgh = 4 if trim(`v') == "4"
            drop `v'
            rename __pay_hgh `v'
            label values `v' vl_pay_hgh
            label variable `v' "Q159. A bag of 50 kg of DAP currently costs 190,000 MK. How much more would you be willing to pay fertilizer that improves long-term soil health?"
    }
}
cap label drop vl_gen_fert
label define vl_gen_fert 1 "Nothing extra" 2 "Up to 10% more" 3 "Up to 25% more" 4 "More than 25%"
ds *gen_fert, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __gen_fert = .
            replace __gen_fert = 1 if trim(`v') == "1"
            replace __gen_fert = 2 if trim(`v') == "2"
            replace __gen_fert = 3 if trim(`v') == "3"
            replace __gen_fert = 4 if trim(`v') == "4"
            drop `v'
            rename __gen_fert `v'
            label values `v' vl_gen_fert
            label variable `v' "Q162. The standard recommended blend for one acre fertilizer would cost 150,000 MK. How much more would you be willing to pay for customized fertilizer compared to standard"
    }
}
cap label drop vl_sub_form
label define vl_sub_form 1 "Cash voucher" 2 "Direct input delivery" 3 "Digital subsidy wallet"
ds *sub_form, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __sub_form = .
            replace __sub_form = 1 if trim(`v') == "1"
            replace __sub_form = 2 if trim(`v') == "2"
            replace __sub_form = 3 if trim(`v') == "3"
            drop `v'
            rename __sub_form `v'
            label values `v' vl_sub_form
            label variable `v' "Q163. Which subsidy format would you prefer?"
    }
}
cap label drop vl_seas_red
label define vl_seas_red 1 "1 season" 2 "2 seasons" 3 "3+ seasons" 4 "None"
ds *seas_red, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __seas_red = .
            replace __seas_red = 1 if trim(`v') == "1"
            replace __seas_red = 2 if trim(`v') == "2"
            replace __seas_red = 3 if trim(`v') == "3"
            replace __seas_red = 4 if trim(`v') == "4"
            drop `v'
            rename __seas_red `v'
            label values `v' vl_seas_red
            label variable `v' "Q165. For the long-term benefit of improving your soil’s natural fertility — meaning better soil structure, increased organic matter, and higher nutrient availability for future crops — how many agricultural seasons would you be willing to reduce or stop using chemical fertilizers"
    }
}
ds *trade_yld, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q167. Would you accept a 10% lower yield this season if it resulted in a 15% increase in your yields per season over the next three years due to improved soil health?"
}
cap label drop vl_w_pref
label define vl_w_pref 1 "A free soil test every year" 2 "A 10% discount on fertilizer each season"
ds *w_pref, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __w_pref = .
            replace __w_pref = 1 if trim(`v') == "1"
            replace __w_pref = 2 if trim(`v') == "2"
            drop `v'
            rename __w_pref `v'
            label values `v' vl_w_pref
            label variable `v' "Q168. Which would you prefer"
    }
}
cap label drop vl_rel_acc
label define vl_rel_acc 1 "Fertilizer subsidy" 2 "Extension services"
ds *rel_acc, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __rel_acc = .
            replace __rel_acc = 1 if trim(`v') == "1"
            replace __rel_acc = 2 if trim(`v') == "2"
            drop `v'
            rename __rel_acc `v'
            label values `v' vl_rel_acc
            label variable `v' "Q169. Would you rather get reliable access to a 50% subsidized fertilizer (50 kg per acre) or guaranteed access to extension services for one season all year?"
    }
}
ds *risk_pred_cons, has(type string)
foreach v of varlist `r(varlist)' {
    gen byte __tmp = .
    replace __tmp = 1 if lower(trim(`v')) == "yes"
    replace __tmp = 0 if lower(trim(`v')) == "no"
    drop `v'
    rename __tmp `v'
    label define yesno 0 "No" 1 "Yes", replace
    label values `v' yesno
    label variable `v' "Q176. However, to conclude we would also like to play a small game with you, where you stand a chance to win soap. Would you be willing to play this game?"
}
cap label drop vl_risk_pref
label define vl_risk_pref 1 "A" 2 "B" 3 "Refused to play"
ds *risk_pref, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __risk_pref = .
            replace __risk_pref = 1 if trim(`v') == "A"
            replace __risk_pref = 2 if trim(`v') == "B"
            replace __risk_pref = 3 if trim(`v') == "Refused to play"
            drop `v'
            rename __risk_pref `v'
            label values `v' vl_risk_pref
            label variable `v' "Q177. OK, I want you to make a choice between two options. If you choose the ﬁrst option I will give you 1 bar of soap straight. For the second option, the number of bars of soap you get will be determined after tossing a coin. If the coin lands heads up, you will not get any soap, but if the coin lands tails up, I will give you 3 bars of soap. What do you choose: A: one bars of soap for sure, or B: a bet that may result in you getting nothing if the coin land heads up or 3 bars of soap when the coin lands tails up."
    }
}
cap label drop vl_bet_outcome
label define vl_bet_outcome 1 "Nothing (heads up)" 2 "3 Bars of soap (tails up)"
ds *bet_outcome, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        gen long __bet_outcome = .
            replace __bet_outcome = 1 if trim(`v') == "1"
            replace __bet_outcome = 2 if trim(`v') == "2"
            drop `v'
            rename __bet_outcome `v'
            label values `v' vl_bet_outcome
            label variable `v' "Record bet outcome and give soap to farmer if tails is up"
    }
}
* Expand select_multiple, keep base text column
ds *stick1, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[I don’t trust the soil test results or recommendations] Q15c. What is the main reason you prefer to stick to your current fertilizer practice?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}

ds *impr_no1, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[IMPROVED SEED WAS TOO EXPENSIVE/NOT PROFITABLE] Q53. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}

ds *expenses1, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[Irrigation equipment] Q87c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}

ds *impr_nor1, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[IMPROVED SEED WAS TOO EXPENSIVE/NOT PROFITABLE] Q116. If improved SEED is NOT at all used on this plot, why?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}

ds *expensesr1, has(type string)
if "`r(varlist)'" != "" {
    foreach v of varlist `r(varlist)' {
        capture confirm string variable `v'
        if !_rc {
            gen byte __tmp = .
            replace __tmp = 1 if lower(trim(`v')) == "true"
            replace __tmp = 0 if lower(trim(`v')) == "false"
            drop `v'
            rename __tmp `v'
        }
        label define yesno 0 "No" 1 "Yes", replace
        label values `v' yesno
local __lbl "[Irrigation equipment] Q149c. What expenses did you have?"
local __lbl : display substr("`__lbl'", 1, 80)
label variable `v' `"`__lbl'"'
    }
}
* ---------- Fertilizer type coding for: test_plot{1..4}fertilizer_name ----------
capture label drop ferttype
label define ferttype ///
    1  "MAP (Monoammonium Phosphate / Technical Grade)" ///
    2  "DAP (Diammonium Phosphate)" ///
    3  "UREA" ///
    4  "Calcium Ammonium Nitrate (CAN)" ///
    5  "NPK 14:14:20 +4S +2 Mg + Zn/B/Cu" ///
    6  "NPK 23:10:5 + 6S + 1Zn (For maize)" ///
    7  "NPK 8:18:15 +6S +0.1B (for tobacco)" ///
    8  "NPK 15:23:16+6S+0.5Zn+0.3B" ///
    9  "SOP (Sulphate of Potash / Potassium Sulphate)" ///
    10 "MOP (Muriate of Potash / Potassium Chloride)" ///
    11 "CALCITIC LIME" ///
    12 "DOLOMITIC LIME" ///
    13 "Potassium Sulphate" ///
    14 "other" ///
    15 "don't know" ///
    16 "Never used  fertilizers"

local fert_vars test_plot1fertilizer_name test_plot2fertilizer_name ///
                 test_plot3fertilizer_name test_plot4fertilizer_name ///
                 rnd_plot1fertilizer_namer rnd_plot2fertilizer_namer ///
                 rnd_plot3fertilizer_namer

foreach v of local fert_vars {
    capture confirm variable `v'
    if !_rc {

        * preserve current variable label (if any)
        local __vlab : variable label `v'

        * If string, map text -> numeric codes; keep same varname
        capture confirm string variable `v'
        if !_rc {
            tempvar __tmp
            gen int `__tmp' = .
            replace `__tmp' = 1  if lower(trim(`v')) == "map"
            replace `__tmp' = 2  if lower(trim(`v')) == "dap"
            replace `__tmp' = 3  if lower(trim(`v')) == "urea"
            replace `__tmp' = 4  if lower(trim(`v')) == "can"
            replace `__tmp' = 5  if lower(trim(`v')) == "npk14:14:20"
            replace `__tmp' = 6  if lower(trim(`v')) == "npk23:10:5"
            replace `__tmp' = 7  if lower(trim(`v')) == "npk8:18:15"
            replace `__tmp' = 8  if lower(trim(`v')) == "npk15:23:16"
            replace `__tmp' = 9  if lower(trim(`v')) == "sop"
            replace `__tmp' = 10 if lower(trim(`v')) == "mop"
            replace `__tmp' = 11 if lower(trim(`v')) == "calcitic_lime"
            replace `__tmp' = 12 if lower(trim(`v')) == "dolomitic_lime"
            replace `__tmp' = 13 if lower(trim(`v')) == "potassium_sulphate"
            replace `__tmp' = 14 if lower(trim(`v')) == "other"
            replace `__tmp' = 15 if lower(trim(`v')) == "dk"
            replace `__tmp' = 16 if lower(trim(`v')) == "never"
            drop `v'
            rename `__tmp' `v'
        }

        * Attach value labels (also works if var was already numeric)
        label values `v' ferttype

        * restore original variable label if it existed
        if "`__vlab'" != "" {
            label variable `v' `"`__vlab'"'
        }
        * else: leave as-is (or set a default if you prefer)
        * label variable `v' "Fertilizer type for plot"   // <- optional
    }
}

* Fill variable labels only when currently missing
cap program drop __fill_varlabel_if_missing
program define __fill_varlabel_if_missing
    // usage: __fill_varlabel_if_missing <varname> "<label text>"
    args v lbl
    capture confirm variable `v'
    if !_rc {
        local cur : variable label `v'
        if "`cur'" == "" {
            label variable `v' `"`lbl'"'
        }
    }
end

*==============================================================*
* Auto-fill / fix variable labels from XLSForm (survey sheet)
* - Uses exact name match first
* - Then suffix match (handles test_plotplot_siz -> plot_siz)
* - Overwrites only when we find a label in the form and it's different
* - Works for non-select questions only (select_one/multiple dropped)
*==============================================================*

* 0) Path to your XLSForm
local xlsform "$raw/S2P_Endline_V3_last_downloaded.xlsx"

*--------------------------------------------------------------*
* 1) Build lookup table: name + label for NON-select questions
*--------------------------------------------------------------*
preserve
    import excel using "`xlsform'", sheet("survey") firstrow clear

    * Normalize names to lower case
    rename *, lower

    * Drop select_one / select_multiple – those you already handle elsewhere
    gen lower_type = lower(type)
    drop if strpos(lower_type, "select_one")      ///
         | strpos(lower_type, "select_multiple")

    * Use the 'label' column from the XLSForm
    capture confirm variable label
    if _rc {
        di as error "No variable called 'label' found in survey sheet. Change this to the correct label column name."
        error 111
    }

    * Keep only what we need
    keep name label
    drop if missing(name) | missing(label)
    rename label __lab

    tempfile _lu
    save "`_lu'"
restore

*--------------------------------------------------------------*
* 2) Loop over all vars; align labels with XLSForm where possible
*    - exact match on name
*    - else suffix match: varname ends with form name
*    - overwrite only if XLSForm label exists & differs
*--------------------------------------------------------------*
ds, has(type numeric string)
foreach v of varlist `r(varlist)' {

    local L ""   // candidate label from XLSForm

    * --- 2a. Exact match: name == varname ---
    preserve
        use "`_lu'", clear
        keep if lower(name) == lower("`v'")
        if _N {
            local L = __lab[1]
        }
    restore

    * --- 2b. Suffix match for repeat vars (e.g. test_plotplot_siz -> plot_siz) ---
    if "`L'" == "" {
        preserve
            use "`_lu'", clear
            gen __v    = lower("`v'")
            gen __name = lower(name)
            gen __ok = (length(__v) >= length(__name)) ///
                & (substr(__v, length(__v) - length(__name) + 1, .) == __name)
            keep if __ok
            if _N {
                local L = __lab[1]
            }
        restore
    }

    * --- 2c. If we found a form label, overwrite only if different ---
    if "`L'" != "" {
        local cur : variable label `v'
        * Trim spaces on both sides for a cleaner comparison
        local curtrim : display trim("`cur'")
        local Ltrim   : display trim("`L'")

        if "`curtrim'" != "`Ltrim'" {
            label variable `v' `"`L'"'
        }
    }
}

* ---- Admin / core fields ----
quietly __fill_varlabel_if_missing today           "Interview date (YYYY-MM-DD)"

quietly __fill_varlabel_if_missing plot_calc1      "Helper calculation 1"
quietly __fill_varlabel_if_missing plot_calc2      "Helper calculation 2"
quietly __fill_varlabel_if_missing plot_select     "Selected plot index"
quietly __fill_varlabel_if_missing order           "Interview order within cluster"
quietly __fill_varlabel_if_missing cluster_id_num  "Cluster numeric ID"
quietly __fill_varlabel_if_missing cluster_id      "Cluster ID"
quietly __fill_varlabel_if_missing treat           "Assigned treatment arm"
quietly __fill_varlabel_if_missing q4a             "Were you able to find  ${farmer_name} ?"



* Destring only variables that are actually string
ds, has(type string)
local strvars `r(varlist)'
foreach v of local strvars {
    quietly destring `v', replace ignore(" ")
}
compress
save "$processed/endline_labeled.dta", replace






*** Improting the recommendation file 
*--------------------------------------------------------------------
* 1. Import Excel file
*--------------------------------------------------------------------
**--------------------------------------------------------------------
* 1. Import soil recommendations Excel file
*--------------------------------------------------------------------
import excel "C:\Users\HTiruneh\IFPRI Dropbox\Hailemariam Ayalew\Space2Place\Analysis\data\raw\recommendations.xlsx", ///
    sheet("recommendations") clear firstrow allstring

*--------------------------------------------------------------------
* 2. Prepare soil_id
*--------------------------------------------------------------------
rename Barcode soil_id
replace soil_id = trim(soil_id)
replace soil_id = subinstr(soil_id, " ", "", .)

*--------------------------------------------------------------------
* 3. Convert all possible variables to numeric
*--------------------------------------------------------------------
ds, has(type string)
local allstringvars `r(varlist)'

ds, has(type string)
local allstringvars `r(varlist)'

local to_convert: list allstringvars - stringvars

foreach v of local to_convert {
    destring `v', replace ignore("Kg/Ha t/Ha , high low optimum")
}

*--------------------------------------------------------------------
* 4. Save temporary cleaned soil rec file
*--------------------------------------------------------------------
tempfile soilrec
inspect soil_id
egen unique_id = group(soil_id)
isid unique_id
sort unique_id
count // 1,322 samples 
save "$processed/recommendation.dta", replace 

*--------------------------------------------------------------------
* 5. Load endline data
*--------------------------------------------------------------------
use "$processed/endline_labeled.dta", clear

*--------------------------------------------------------------------
* 6. Clean soil_id in endline for merging
*--------------------------------------------------------------------
replace soil_id = trim(soil_id)
replace soil_id = subinstr(soil_id, " ", "", .)
egen unique_id = group(soil_id)
inspect unique_id
sort unique_id
drop if unique_id==. // 98 observations are deleted 
duplicates report unique_id // 126 duplicates 
duplicates drop unique_id, force 
isid unique_id
count // 1792

*--------------------------------------------------------------------
* 7. Merge datasets
*--------------------------------------------------------------------
merge 1:1 unique_id using "$processed/recommendation.dta"

*--------------------------------------------------------------------
* 8. Inspect merge results
*--------------------------------------------------------------------
tab _merge
bys treat: tab _merge // There are plots in the control group that received recommendations—around 71 of them (this could be due to incorrect IDs).
