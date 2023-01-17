*This .do file will collapse elections that occur on the same day.  It does not collapse multi-round elections.  
*THIS FILE ASSUMES THAT ALL VARIABLE CODINGS FOR PRESIDENTIAL ELECTIONS ALSO APPLY TO GENERAL ELECTIONS. 
*Researchers should think carefully about whether they  are using NELDA variables that THEORETICALLY vary between Presidential and Legislative elections when they are held on the same day.  
*This method of combining "general elections" will drop responses for the Legislative election
*For example, questions about the incumbent should not be collapsed without careful attention to variation in how these variables may differ between legislative and presidential elections. 
*Researchers can adjust this do file appropriately by sorting on the variables of interest or dropping by hand after dropping only those observation that are duplicated across all variables of interest. 


generate NELDAyear=substr(electionid, 5, 4)
generate NELDAmmdd=substr(electionid, 10, 4)
generate NELDAround=substr(electionid, 16, 1)
generate NELDAccode=substr(electionid, 1, 3)
generate NELDAtype=substr(electionid, 15, 1)

move NELDAccode electionid
move NELDAyear electionid
move NELDAmmdd electionid
move NELDAround electionid
move NELDAtype electionid

*destring  NELDAccode, replace
*destring  NELDAyear, replace
*destring  NELDAmmdd, replace
*destring  NELDAround, replace
*replace ccode=NELDAccode if ccode==.
*edit if ccode==NELDAccode
*replace mmdd=NELDAmmdd if NELDAmmdd==.
*edit if mmdd~=NELDAmmdd & NELDAround==1

duplicates tag NELDAmmdd NELDAccode NELDAyear NELDAround, generate(samedaydupes)
gsort samedaydupes -NELDAtype
generate electionid2=""
replace electionid2=electionid if samedaydupes==0
replace electionid2=NELDAccode+"-"+NELDAyear+"-"+NELDAmmdd+"-"+"G"+NELDAround if samedaydupes==1
replace electionid=NELDAccode+"-"+NELDAyear+"-"+NELDAmmdd+"-"+NELDAtype+NELDAround

duplicates drop electionid2, force
