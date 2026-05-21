*Run master and data cleaning do files 
do Master.do 

*Fixing the duplicate barcodes 
	****************************************************
	* 1. Load the main dataset
	****************************************************
	use "$processed/estimation_data_v1.dta", clear

	* Keep a copy of the original barcode
	gen Barcode_original = Barcode

	tempfile main
	save `main'

	****************************************************
	* 2. Import the barcode correction file from Excel
	****************************************************
	import excel "$raw/Duplicates and matching issues/Main_data_Duplicates.xlsx", sheet("Sheet1") firstrow clear

	* Keep only the variables needed
	keep farmer_id Barcode CorrectBarcode

	rename Barcode Barcode_duplicate
	rename CorrectBarcode Barcode_correct_raw

	****************************************************
	* 3. Clean the correction variable
	****************************************************
	replace Barcode_correct_raw = trim(Barcode_correct_raw)

	* Treat "Correct" and typo variants as meaning: no change needed
	gen Barcode_corrected = Barcode_duplicate
	replace Barcode_corrected = Barcode_correct_raw ///
		if !inlist(lower(Barcode_correct_raw), "correct", "corrrect") ///
		& Barcode_correct_raw != ""

	tempfile fixes
	save `fixes'

	****************************************************
	* 4. Merge corrections into the main data
	****************************************************
	use `main', clear
	merge 1:1 farmer_id using `fixes'

	* Check merge result
	tab _merge

	****************************************************
	* 5. Update barcode where a corrected value exists
	****************************************************
	replace Barcode = Barcode_corrected if _merge == 3 & Barcode_corrected != ""

	****************************************************
	* 6. Create audit variable
	****************************************************
	gen barcode_changed = Barcode != Barcode_original if !missing(Barcode_original, Barcode)

	****************************************************
	* 7. Final validation checks
	****************************************************
	* Check whether duplicate barcodes still exist
	duplicates report Barcode

	* Check whether any farmer has more than one barcode
	duplicates report farmer_id Barcode

	****************************************************
	* 8. Save cleaned file
	****************************************************
	tempfile main_duplicates_fixed
	save `main_duplicates_fixed'
	
*Fixing the missing barcodes 
	****************************************************
	use `main_duplicates_fixed', clear

	gen Barcode_before_fill = Barcode
	drop _merge

	tempfile main2
	save `main2'

	****************************************************
	* 1. Import REC source
	****************************************************
	import excel "$raw/Duplicates and matching issues/Missing_Barcode.xlsx",  firstrow clear

	* Adjust variable names if needed after describe
	keep farmer_id  BarcodeinRecfile BarcodeinListingdata
	rename BarcodeinRecfile Barcode_rec
	rename BarcodeinListingdata Barcode_list

	replace Barcode_rec = trim(Barcode_rec)
	replace Barcode_list = trim(Barcode_list)

	* Check whether the two barcode sources agree
	gen barcode_match = Barcode_rec == Barcode_list ///
    if !missing(Barcode_rec) & !missing(Barcode_list)
	
	tab barcode_match //all match

	* Create one final barcode variable from the two sources
	gen Barcode_fill = Barcode_rec
	replace Barcode_fill = Barcode_list if missing(Barcode_fill) & !missing(Barcode_list)

	tempfile fillsource
	save `fillsource'

	****************************************************
	* 4. Merge fill source into main data
	****************************************************
	use `main2', clear
	merge m:1 farmer_id using `fillsource'

	tab _merge
	drop _merge

	****************************************************
	* 5. Fill missing barcodes only
	****************************************************
	replace Barcode = Barcode_fill if missing(Barcode) & !missing(Barcode_fill)

	****************************************************
	* 6. Audit what was filled
	****************************************************
	gen barcode_filled = missing(Barcode_before_fill) & !missing(Barcode)

	tab barcode_filled

	list farmer_id Barcode_before_fill Barcode Barcode_rec Barcode_list ///
		if barcode_filled == 1, noobs sepby(farmer_id)

	****************************************************
	* 7. Final checks
	****************************************************
	count if missing(Barcode)

	duplicates tag Barcode, gen(dup)
	duplicates report farmer_id Barcode
	drop dup

	****************************************************
	* 8. Save final data
	****************************************************
	save "$processed/estimation_data_v1B.dta", replace

