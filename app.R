
library(shiny)
library(shinyjs)
library(googlesheets4)
library(hash)
library(tidyverse)
library(scales)

# Connecting to Maine Google Sheets data.

maine_localities <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1IQAY_v6OBJJR2hR--XzEX9AuSFPG6aq8KDQ9HUFmWyY/edit#gid=0"))

counties <- unique(maine_localities$"countyname")
towns <- unique(maine_localities$"name")

#counties <- 1:4
#towns <- 1:4

# Helper functions.

pos_sub <- function(var1, var2) {
    result = var1 - var2
    if (result < 0) {
        return(0)
    } else {
        return(result)
    }
}

ceil <- function(x) {
    x <- round(x, -3) + 1000
    return(x)
}

# Checks that certain inputs dependent on others (i.e. two parents will ask the user to add value of a second car) are not null; if they are null, returns 0.
nullCheck <- function(variable) {
    if (is.null(variable)) {
        return(0)
    } else {
        return(variable)
    }
}

lengthCheck <- function(variable) {
    if (length(variable)) {
        return(variable)
    } else {
        return(0)
    }
}

# Named list of outputs. Each module will add a "key", or named item, to this list, so that all of the modules' outputs are accessible in a single list.

outputs_hash <- hash()
outputs_hash[[ "child_support_recd" ]] <- 0
outputs_hash[[ "interest" ]] <- 0
outputs_hash[[ "tanf_recd "]] <- 0
outputs_hash[[ "fsp_recd" ]] <- 0
outputs_hash[[ "liheap_recd" ]] <- 0
outputs_hash[[ "heap_recd" ]] <- 0
outputs_hash[[ "federal_tax_credits" ]] <- 0
outputs_hash[[ "state_tax_credits" ]] <- 0
outputs_hash[[ "local_tax_credits" ]] <- 0
outputs_hash[[ "tax_before_credits" ]] <- 0
outputs_hash[[ "payroll_tax" ]] <- 0
outputs_hash[[ "rent_paid" ]] <- 0
outputs_hash[[ "child_care_expenses" ]] <- 0
outputs_hash[[ "food_expenses" ]] <- 0
outputs_hash[[ "trans_expenses" ]] <- 0
outputs_hash[[ "other_expenses" ]] <- 0
outputs_hash[[ "health_expenses" ]] <- 0
outputs_hash[[ "debt_payment" ]] <- 0

# Define functions (modules in Perl) to be run in this code.

# Interest, 2020.
interest_module <- function(savings, passbook_rate) {
    savings <- as.integer(savings)
    # passbooK_rate <- 0.60
    interest <- 0
    interest_m <- 0
    
    #Interest is derived from the amount entered for family's savings in the user-interface	on Step	3.	The	FRS	utilizes the U.S. Department of	Housing	and	Urban Development's	(HUD) method	for	calculating	interest: when a family has net assets over $5,000,	we apply the national average interest rate, also known as the passbook	savings rate. Savings equal to or less than $5,000 are	assumed	not to accrue interest. The passbook savings rate was 0.06 in 2019, unchanged from 2016, last announced at https://www.hud.gov/sites/documents/16-01HSGN.PDF. As with HUD standards, this rate is applied annually, and is not compounded monthly or quarterly. We assume the family does not have any other investments.	
    
    if (savings <= 5000) {
        interest <- 0
    } else {
        interest <- passbook_rate * savings
    }
    
    interest_m <- interest / 12
    
    outputs_hash[[ "interest" ]] <<- interest
    outputs_hash[[ "interest_m" ]] <<- interest_m
}

# Parent earnings, 2020.
parent_earnings_module <- function(earnings, family_structure, nontraditionalwork, parent1_first_max, parent1_max_work, parent2_max_work_override_amt, parent2_max_work, maxshiftlength_parent1, maxshiftlength_parent2, maxworkweek_parent1, maxworkweek_parent2, backtobackshifts_parent1, backtobackshifts_parent2, wage_1, wage_parent2) {
    
    family_structure <- as.integer(family_structure)
    parent1_first_max <- as.integer(parent1_first_max)
    parent1_max_work <- as.integer(parent1_max_work)
    parent2_max_work_override_amt <- as.integer(parent2_max_work_override_amt)
    parent2_max_work <- as.integer(parent2_max_work)
    maxshiftlength_parent1 <- as.integer(maxshiftlength_parent1)
    maxshiftlength_parent2 <- as.integer(maxshiftlength_parent2)
    maxworkweek_parent1 <- as.integer(maxworkweek_parent1)
    maxworkweek_parent2 <- as.integer(maxworkweek_parent2)
    backtobackshifts_parent1 <- as.integer(backtobackshifts_parent1)
    backtobackshifts_parent2 <- as.integer(backtobackshifts_parent2)
    wage_1 <- as.numeric(wage_1)
    wage_parent2 <- as.numeric(wage_parent2)
    
    # Establishing outputs.
    parent1_employed_hours_w <- 0 # number of hours/week parent 1 works in paid employment
    parent2_employed_hours_w <- 0 # number of hours/week parent 2 works in paid employment
    parent1_transhours_w <- 0 # This variable is unchanged in this code, but we need to establish it as 0 here in order for the revised child care codes to work successfully. The child care module will likely be run twice, first before the work module recalculates this variable based on tanf participation and then again, after. Changes in these variables are indicators of participation in TANF work requirements, so the child care module will be able to detect  this change the second time it is run.
    parent2_transhours_w <- 0 # same justification as parent1_transhours above
    parent1_earnings <- 0 # parent 1's earnings per year
    parent2_earnings <- 0 # parent 2's earnings per year
    parent1_earnings_m <- 0 # parent 1's earnings per month
    parent2_earnings_m <- 0 # parent 2's earnings per month
    parent_workhours_w <- 0
    caregiver_workshifts_w <- 0
    caregiver_maxworkweek <- 0
    caregiver_maxshiftlength <- 0
    caregiver_backtobackshifts <- 0
    parent_otherhours_w <- 0
    
    # Other variables used in this function.
    wage1_annualized <- 0 # hourly wage rate * 52 weeks/year
    wage2_annualized <- 0 # hourly wage rate * 52 weeks/year
    parent2_max_hours_w <- 0 # max hours worked by second parent per week
    parent1_fulltime_earn <- 0 # annual earnings of first parent at the point at which the parent reaches maximum-time employment (ie, 35 hours/week, 52 weeks/year)
    parent2_maxtime_earn <- 0 # annual earnings of second parent at the point at which the parent reaches maximum-time employment
    parent1_first_earn <- 0 # This is the amount that the first parent earns after which the second parent begins working.
    firstrunchildcare <- 0 # I think we need something like this in order for the child care codes to run either if it’s the first time through or if tanf work requirements need to be enacted.
    tanflock <- 0 # This may be necessary to avoid a nonsensical scenario during the second run of tanf, depending on what state-level policies use tanf receipt as an input. Developed in 2017 because necessary for proper modeling of DC policies.
    shifts_parent1 <- 0
    shifts_parent2 <- 0
    multipleshifts_parent1 <- 0
    multipleshifts_parent2 <- 0
    
    # We now also add zero-value definitions for a slew of variables that need to be defined early in the code in order for the loop incorporating child support, child care, tanf, and tanf work requirements to run correctly. See tanf code for further explanation on this.
    
    child_support_paid <- 0 # Since we will need to invoke these output variables in the child_support code, we need to establish them as output variables in earlier code, e.g. here.
    child_support_paid_m <- # Since we will need to invoke these output variables in the child_support code, we need to establish them as output variables in earlier code, e.g. here.
    tanf_recd <- 0
    tanf_recd_m <- 0
    child_support_recd <- 0
    child_support_recd_m <- 0
    parent2_incapacited <- 0
    tanf_family_structure <- 0
    unit_size <- 0
    stipend_amt <- 0
    tanflock <- 0
    tanf_sanctioned_amt <- 0
    fsp_recd <- 0
    
    # Other variables.
    breadwinner_wkday_hometime <- 0
    breadwinner_wkend_hometime <- 0
    
    # This is where we set the variables that feed into the nontraditional work schedule modeling to default (traditional) values, if the user has selected that family members work traditional schedules. 
    if (nontraditionalwork == "Yes") {
        parent1_max_work <- 40
        maxshiftlength_parent1 <- 8
        maxworkweek_parent1 <- 5
        backtobackshifts_parent1 <- 0
        weekenddaysworked <- 0
        maxweekendshifts <- 0
        workdaystart <- 9
        maxshiftlength_parent2 <- 8
        maxworkweek_parent2 <- 5
        parent1_first_max <- 40
        backtobackshifts_parent2 <- 0
        breadwinner_wkday_hometime <- 0
        breadwinner_wkend_hometime <- 0
    }

    #Determine maximum hours worked by second parent.
    
    if (family_structure == 1) {
        parent2_max_hours_w <- 0
        parent1_first_max <- parent1_max_work
    } else if (isTRUE(parent2_max_work_override_amt == 1) & nontraditionalwork == "Yes") {
        parent2_max_hours_w <- parent2_max_work_override_amt
    } else {
        if (isTRUE(parent2_max_work == "not_employed")) {
            parent2_max_hours_w <- 0
        } else if (isTRUE(parent2_max_work == "part_time")) {
            parent2_max_hours_w <- 20
        } else {
            parent2_max_hours_w <- 40
        }
    }
    
    #Determine parents' work hours and earnings.
    
    #print(wage_1)
    #print(wage_parent2)
    
    wage1_annualized <- wage_1 * 52
    wage2_annualized <- wage_parent2 * 52
    parent1_first_earn <- wage1_annualized * parent1_first_max
    parent1_fulltime_earn <- wage1_annualized * parent1_max_work
    parent2_maxtime_earn <- wage2_annualized * parent2_max_hours_w
    
    if (earnings <= parent1_first_earn) {
        parent1_employed_hours_w <- earnings / wage1_annualized
        parent1_earnings <- earnings
        parent2_employed_hours_w <- 0
        parent2_earnings <- 0
    } else if (isTRUE(earnings <= (parent1_first_earn + parent2_maxtime_earn))) {
        parent1_employed_hours_w <- parent1_first_max
        parent1_earnings <- parent1_first_earn
        parent2_earnings <- earnings - parent1_earnings
        parent2_employed_hours_w <- parent2_earnings / wage2_annualized
    } else if (isTRUE(earnings <= (parent1_first_earn + parent2_maxtime_earn))) {
        parent1_employedhours_w <- parent1_first_max
        parent1_earnings <- parent1_first_earn
        parent2_employed_hours_w <- parent2_max_hours_w
        parent2_earnings <- parent2_maxtime_earn
    } else {
        parent2_employed_hours_w <- parent2_max_hours_w
        parent2_earnings <- parent2_maxtime_earn
        parent1_earnings <- earnings - parent2_earnings
        parent1_employed_hours_w <- parent1_max_work
    }
    
    parent1_earnings_m <- parent1_earnings / 12
    parent2_earnings_m <- parent2_earnings / 12
    parent1_transhours_w <- parent1_employed_hours_w
    parent2_transhours_w <- parent2_employed_hours_w
    shifts_parent1 <- parent1_transhours_w / maxshiftlength_parent1
    shifts_parent2 <- parent2_transhours_w / maxshiftlength_parent2
    
    if (family_structure == 1) {
        parent_workhours_w <- parent1_employed_hours_w
        caregiver_workshifts_w <- shifts_parent1
        caregiver_maxworkweek <- maxworkweek_parent1
        caregiver_maxshiftlength <- maxshiftlength_parent1
        caregiver_backtobackshifts <- backtobackshifts_parent1
    } else {
        parent_workhours_w <- min(parent2_employed_hours_w, parent1_employed_hours_w)
        if (parent1_transhours_w >= parent2_transhours_w) {
            parent_otherhours_w <- parent1_transhours_w
            caregiver_workshifts_w <- shifts_parent2
            caregiver_maxworkweek <- maxworkweek_parent2
            caregiver_maxshiftlength <- maxshiftlength_parent2
            caregiver_backtobackshifts <- backtobackshifts_parent2
        } else {
            parent_otherhours_w <- parent2_transhours_w
            caregiver_workshifts_w <- shifts_parent1
            caregiver_maxworkweek <- maxworkweek_parent1
            caregiver_maxshiftlength <- maxshiftlength_parent1
            caregiver_backtobackshifts <- backtobackshifts_parent1
        }
    }
    
    outputs_hash[[ 'firstrunchildcare' ]] <<- firstrunchildcare
    outputs_hash[[ 'parent1_employed_hours_w' ]] <<- parent1_employed_hours_w
    outputs_hash[[ 'parent2_employed_hours_w' ]] <<- parent2_employed_hours_w
    outputs_hash[[ 'parent1_earnings' ]] <<- parent1_earnings
    outputs_hash[[ 'parent2_earnings' ]] <<- parent2_earnings
    outputs_hash[[ 'parent1_earnings_m' ]] <<- parent1_earnings_m
    outputs_hash[[ 'parent2_earnings_m' ]] <<- parent2_earnings_m
    outputs_hash[[ 'parent1_transhours_w' ]] <<- parent1_transhours_w
    outputs_hash[[ 'parent2_transhours_w' ]] <<- parent2_transhours_w
    outputs_hash[[ 'shifts_parent1' ]] <<- shifts_parent1
    outputs_hash[[ 'shifts_parent2' ]] <<- shifts_parent2
    outputs_hash[[ 'multipleshifts_parent1' ]] <<- multipleshifts_parent1
    outputs_hash[[ 'multipleshifts_parent2' ]] <<- multipleshifts_parent2
    outputs_hash[[ 'parent_otherhours_w' ]] <<- parent_otherhours_w
    outputs_hash[[ 'caregiver_workshifts_w' ]] <<- caregiver_workshifts_w
    outputs_hash[[ 'caregiver_maxworkweek' ]] <<- caregiver_maxworkweek
    outputs_hash[[ 'caregiver_maxshiftlength' ]] <<- caregiver_maxshiftlength
    outputs_hash[[ 'caregiver_backtobackshifts' ]] <<- caregiver_backtobackshifts
    outputs_hash[[ 'parent_workhours_w' ]] <<- parent_workhours_w
    outputs_hash[[ 'child_support_paid' ]] <<- child_support_paid
    outputs_hash[[ 'child_support_paid_m' ]] <<- child_support_paid_m
    outputs_hash[[ 'tanf_recd' ]] <<- tanf_recd
    outputs_hash[[ 'tanf_recd_m' ]] <<- tanf_recd_m
    outputs_hash[[ 'child_support_recd' ]] <<- child_support_recd
    outputs_hash[[ 'child_support_recd_m' ]] <<- child_support_recd_m
    outputs_hash[[ 'parent2_incapacited' ]] <<- parent2_incapacited
    outputs_hash[[ 'tanf_family_structure' ]] <<- tanf_family_structure
    outputs_hash[[ 'unit_size' ]] <<- unit_size
    outputs_hash[[ 'stipend_amt' ]] <<- stipend_amt
    outputs_hash[[ 'tanflock' ]] <<- tanflock
    outputs_hash[[ 'tanf_sanctioned_amt' ]] <<- tanf_sanctioned_amt
    outputs_hash[[ 'fsp_recd' ]] <<- fsp_recd
    
}

# State Supplemental Program, Maine 2020.
ssp_module <- function() {
    # Some states supplement the federal SSI benefit with their own state benefit, which increases cash assistance for individuals eligible for SSI or ineligible due to incomes that slightly exceed SSI standards. To keep the SSI module applicable across different states, we are adding a separate module beginning in 2020 that tracks state SSI supplements (called the "state supplementary program", or SSP) in states that have them. Although small in amount, receiving this supplement can also allow individuals with disabilities at slightly higher incomes to receive Medicaid. Kentucky indeed provides an SSP, but it is limited to individuals with disabilities requiring in-home care or living in a licensed facility serving people with disabilities. We are not yet including this as an option for the FRS, so the potential benefit from Kentucky's SSP program will be $0 unless we reassess our living arrangement options. See https://chfs.ky.gov/agencies/dcbs/dfs/Documents/OMVOLV.pdf. 
    # This code is also so simple and is purely a hard-coded assignment of state variables; it may be easier to eventually include this as a variable in the db.
    # These SSP amounts and variables are incorporated into the federal SSI Perl code.
    
    ssp_couple_thresh <- 12 # Amount the state's SSP program is structured increases the income standard above the federal threshold for couples, to provide supplemental funds to cover otherwise inelgible families.
    ssp_individual_thresh <- 8 # Amount the state's SSP program is structured increases the income standard above the federal threshold for individuals, to provide supplemental funds to cover otherwise inelgible individuals 
    ssp_couple_ben <- 0 # Amount state's SSP program increase benefits for couples resceiving federal SSI supports.
    ssp_individual_ben <- 0 # Amount state's SSP program increase benefits for individuals receiving federal SSI supports.
    
    outputs_hash[[ "ssp_couple_thresh"]] <<- 12
    outputs_hash[[ "ssp_individual_thresh" ]] <<- 8
    outputs_hash[[ "ssp_couple_ben" ]] <<- 0
    outputs_hash[[ "ssp_individual_ben" ]] <<- 0
}

# Supplemental Security Income, 2020.
ssi_module <- function(ssi, savings, vehicles, family_structure, disability_parent1, disability_parent2, disability_work_expenses_m, child_number, interest_m, earnings_mnth, parent1_earnings, parent2_earnings, ssp_couple_ben, ssp_individual_ben, ssp_couple_thresh, ssp_individual_thresh) {
    
    savings <- as.integer(savings)
    
    family_structure <- as.integer(family_structure)
    disability_parent1 <- as.integer(disability_parent1)
    disability_parent2 <- as.integer(disability_parent2)
    disability_work_expenses_m <- lengthCheck(as.integer(disability_work_expenses_m))
    child_number <- as.integer(child_number)
    interest_m <- as.integer(interest_m)
    earnings_mnth <- as.integer(earnings_mnth)
    parent1_earnings <- as.integer(parent1_earnings)
    parent2_earnings <- as.integer(parent2_earnings)
    
    # Establishing outputs.
    fbr_couple <- 1175              # monthly max SSI benefit for couple
    fbr_individual <- 783           # monthly max SSI benefit for individual
    ssi_couple_asset_limit <- 3000  # SSI asset limit for couples
    ssi_indiv_asset_limit <- 2000   # SSI asset limit for individuals
    ssi_assets <- 0                 # SSI asset calculation
    ssi_income <- 0                 # Countable income according to SSI rules
    ssi_recd_mnth <- 0              # monthly SSI payment
    ssi_recd <- 0                   # yearly SSI payment
    parent1ssi_recd <- 0
    parent2ssi_recd <- 0
    deemed_child_allocation <- 0    # Income deemed to children (excluded from SSI income)
    eligible_parent_earnings <- 0   # Delineates disabled parent earnings from non-disabled parent earnings.
    ineligible_parent_earnings <- 0 # Delineates non-disabled parent earnings from disabled  parent earnings.
    ineligible_parent_unearned_income <- 0 # Separates non-disabled parent’s unearned income based on SSI eligibility formula.
    ineligible_parent_earned_income <- 0 # Separates non-disabled parent’s earned income  # based on SSI eligibility formula.
    
    # We first adjust the federal thresholds based on any state-administered SSP threshold adjustments:
    
    fbr_couple <- fbr_couple + ssp_couple_thresh
    fbr_individual <- fbr_individual + ssp_individual_thresh
    
    if (ssi == 0) {
        ssi_recd_mnth <- 0
        ssi_recd <- 0
    } else if (disability_parent1 == 0 & disability_parent2 == 0) {
        ssi_recd_mnth <- 0
        ssi_recd <- 0
    } else {
        
        # For parents, from https://www.ssa.gov/OP_Home/ssact/title16b/1600.htm, https://www.ssa.gov/ssi/text-understanding-ssi.htm.  
        
        # Resource and disability test
        # One vehicle is excluded regardless of value if used for transportation for you or member of your household. The below formula would therefore only count the lowest value vehicle in the asset calculation.
        
        # One vehicle is excluded. According to https://secure.ssa.gov/poms.nsf/lnx/0501130200, exclude vehicles such that the exclusion is most advantageous to the recipient. These instructions also indicate that vehicle equity (not current market value) is used for this determination. 
        
        if (length(vehicles) / 2 == 2) {
            ssi_assets <- savings + min(lengthCheck(vehicles[[ "vehicle1_value" ]] - vehicles[[ "vehicle1_owed" ]]), 
                                        lengthCheck(vehicles[[ "vehicle2_value" ]] - vehicles[[ "vehicle2_owed"]]))
        } else {
            ssi_assets <- savings
        }
        
        
        if (family_structure == 1) {
            # These limits have remained unchanged since 1989.
            # single-parent unit
            applicable_asset_limit <- ssi_indiv_asset_limit
        } else {
            # two-parent unit (All resources from a family unit are deemed resources, regardless of whether they can be attributed to an individual eligible or ineligible for SSI.)
            applicable_asset_limit <- ssi_couple_asset_limit
        } 
        
        if (ssi_assets > applicable_asset_limit) {
            ssi_recd_mnth <- 0
        } else {
            
            # Determination based on income (from https://www.ssa.gov/ssi/text-income-ussi.htm) :

            # First $20 received in each month “of most income received in a month” is a disregard. Another $65 is also deducted from earnings, and half that is also deducted. Additional expenses needed for a disabled adult to get to work (Impairment-Related Work Expenses) can also be deducted.  The below series of pos_sub commands allows $20 to be deducted first from unearned income and then, if $20 exceeds unearned income, the remainder to be applied to unearned income. $65 of earned income is excluded before an exclusion of half of that remainder is applied.
            
            # Scenario 1: single disabled parent, no spouse:
            if (family_structure == 1) {
                ssi_income <- pos_sub(pos_sub(interest_m, 20) + 0.5 * pos_sub(earnings_mnth, (65 + pos_sub(20, interest_m))), 
                                      disability_work_expenses_m)
                ssi_recd_mnth <- pos_sub(fbr_individual + ssp_individual_ben, ssi_income)
                parent1ssi_recd <- ssi_recd_mnth * 12
            } else if (disability_parent1 == 1 & disability_parent2 == 1) {
                
                # Scenario 2: two disabled parents:
                ssi_income <- pos_sub(pos_sub(interest_m, 20) + 0.5 * pos_sub(earnings_mnth, (65 + pos_sub(20, interest_m))),
                                      disability_work_expenses_m)
                ssi_recd_mnth <- pos_sub(fbr_couple + ssp_couple_ben, ssi_income)
                parent1ssi_recd <- ssi_recd_mnth * 12 / 2
                parent2ssi_recd <- ssi_recd_mnth * 12 / 2
            } else {
                
                # Scenario 3: One parent is disabled, the other is not.
                # We now follow the steps for deeming income from an ineligible spouse. From https://secure.ssa.gov/poms.nsf/lnx/0501320400, it appears that child support is not included as income to the ineligible child for the purposes of reducing the ineligible child allocation. There does not seem to be any deeming calculated for the income of single eligible parents – or income of children – toward ineligible children. https://www.ssa.gov/policy/docs/issuepapers/ip2003-01.html, https://www.ssa.gov/OP_Home/ssact/title16b/1600.htm, and http://www.worksupport.com/documents/parentChildDeemFeb08.pdf are also helpful.
                
                deemed_child_allocation <- child_number * (fbr_couple - fbr_individual)
                
                # I think we can assume that families who have people with disabilities transfer all interest-generating accounts to the non-disabled individual, in order to maximize their SSI receipt. Therefore, all interest will be considered unearned income for any non-disabled parents. We need to make a note of this in our list of assumptions.
                
                # In order to make this work, and to generalize this so that we can use efficient code to describe two different situations (one where parent1 is disabled but not parent2, and the other where parent2 is disabled but not parent1), we can use the following shortcut:
                
                if (disability_parent1 == 1) {
                    eligible_parent_earnings <- parent1_earnings / 12
                    ineligible_parent_earnings <- parent2_earnings / 12
                } else {
                    eligible_parent_earnings <- parent2_earnings / 12
                    ineligible_parent_earnings <- parent1_earnings / 12
                }
                
                # The child allocation is subtracted from the ineligible’s parent’s unearned income, and any remainder is applied to their earned income.
                
                ineligible_parent_unearned_income <- pos_sub(interest_m, deemed_child_allocation)
                
                if (deemed_child_allocation > interest_m) {
                    ineligible_parent_earned_income <- pos_sub(ineligible_parent_earnings, deemed_child_allocation - interest_m)
                } else {
                    ineligible_parent_earned_income <- ineligible_parent_earnings
                }
                
                # When the remaining income is lower than the difference between the FBR for a couple and the FBR for an individual, there is no income to deem from the ineligible spouse to the eligible individual, and only the eligible individual’s income is considered for SSI eligibility and receipt (assuming each parent’s incomes are consistent across all months in a year). Also, since we are considering all interest will be held by the ineligible parent, there is only earned income to consider:
                
                if (ineligible_parent_unearned_income + ineligible_parent_earned_income <= fbr_couple - fbr_individual) {
                    ssi_income <- pos_sub(0.5 * pos_sub(eligible_parent_earnings, 85), disability_work_expenses_m)
                    ssi_recd_mnth <- pos_sub(fbr_individual + ssp_individual_ben, ssi_income) # Adding SSP variable here is new for 2019. The SSP policy for PA works this way, but it may be more complicated for other states.
                } else {
                    # Deeming applies when remaining income is higher than the difference between the FBR for a couple and the FBR for an individual. They are treated as an eligible couple, but with the ineligible parent’s income lowered based on the deeming above.
                    ssi_income <- pos_sub(ineligible_parent_unearned_income, 20) + 0.5 * pos_sub(ineligible_parent_earned_income + eligible_parent_earnings, 65) + pos_sub(20, ineligible_parent_unearned_income) + disability_work_expenses_m
                    ssi_recd_mnth <- pos_sub(fbr_couple + ssp_couple_ben, ssi_income) # Adding SSP variable here is new for 2019. The SSP policy for PA works this way, but it may be more complicated for other states.
                }
                
                if (disability_parent1 == 1) {
                    parent1ssi_recd <- ssi_recd_mnth * 12
                } else {
                    parent2ssi_recd <- ssi_recd_mnth * 12
                }
                
                # Note: “The SSI benefit under these deeming rules cannot be higher than it would be if deeming did not apply,” but for the variables we are considering, this would never happen. It could happen if earnings are inconsistent between months. 
                
            }
        }
        
    }
    
    ssi_recd <- 12 * ssi_recd_mnth
    
    # There are also state supplements to SSI. According to https://www.ssa.gov/ssi/text-benefits-ussi.htm, the federal government operates the DC state supplement.
    
    # Children are also eligible for SSI, and while modeling SSI benefits for children is possible, it would not be realistic to include them in this model without also including a variety of other benefits aimed at children with disabilities. We are forgoing their inclusion at this time. See https://www.ssa.gov/ssi/text-child-ussi.htm and other related pages if considering including children in the future.
    
    # Outputs handled below.
    
    outputs_hash[[ 'ssi_recd' ]] <<- ssi_recd
    outputs_hash[[ 'ssi_recd_mnth' ]] <<- ssi_recd_mnth
    outputs_hash[[ 'parent1ssi_recd' ]] <<- parent1ssi_recd
    outputs_hash[[ 'parent2ssi_recd' ]] <<- parent2ssi_recd

}

# Federal Health Insurance, 2020.
fed_hlth_insurance_module <- function(earnings_mnth, interest_m, family_size, disability_parent1, disability_parent2, ssi_recd, parent1_earnings, parent2_earnings) {
    
    interest_m <- as.integer(interest_m)
    family_size <- as.integer(family_size)
    disability_parent1 <- as.integer(disability_parent1)
    disability_parent2 <- as.integer(disability_parent2)
    ssi_recd <- as.integer(ssi_recd)
    parent1_earnings <- as.integer(parent1_earnings)
    parent2_earnings <- as.integer(parent2_earnings)
    
    max_income_pct_employer <- 0.0986 #  Maximum percentage of income dedicated for self-coverage under employer-based. This is the required contribution percentage for the 2019 tax year, as indicated at https://www.irs.gov/pub/irs-drop/rp-18-34.pdf... plan in order to be ineligible for marketplace coverage
    magi_disregard <- 0.05 #  MAGI  disregard (as % of FPG)
    sub_minimum <- 1.0 #  Minimum % of income compared to poverty to be eligible for the premium tax credit.
    sub_maximum <- 4.0 #  Maximum % of income compared to poverty to be eligible for the premium tax credit.
    medically_needy <- 0 #  This is a variable that we're introducing to the 2019 fed health code because some states (like Florida) have Medically Needy provisions in their Medicaid policies, which confer categorical eligibilty to federal progams like Lifeline. In order to make these other codes (e.g. Lifeline)  work across states (including Florida), it's important to define this variable somewhere. For states without a medically needy program, it will just stay at 0.
    
    # Outputs.
    hlth_gross_income_m <- 0        #   Modified Adjusted Gross Income
    private_max <- 0                #   Maximum payment toward coverage of Second Lowest Priced Silver Plan
    percent_of_poverty <- 0         #   Income as representative of  percent of applicable poverty level
    
    # Other variables.
    subsidy_pov_levels <- c(1041, 1409, 1778, 2146, 2514, 2883, 3251) # Lists in R start with index = 1.
    subsidy_pov_level_m <- subsidy_pov_levels[family_size]
    # Note about the poverty guidelines used to determine eligibility for premium tax credits: The sources for these 2019 values are https://www.irs.gov/affordable-care-act/individuals-and-families/eligibility-for-the-premium-tax-credit, https://www.irs.gov/affordable-care-act/individuals-and-families/questions-and-answers-on-the-premium-tax-credit, https://www.healthcare.gov/blog/when-is-2019-open-enrollment/, https://www.federalregister.gov/documents/2019/02/01/2019-00621/annual-update-of-the-hhs-poverty-guidelines and https://www.federalregister.gov/documents/2018/01/18/2018-00814/annual-update-of-the-hhs-poverty-guidelines. These IRS documents  indicate that the applicable federal poverty level is the most recently published poverty level at the beginning of the open enrollment period, adn the Q&A document indicates that for 2018, hte most recently published levels were the 2017 federal poverty level. The open enrollment period for 2019 marketplace plans began on 11/1/2018, according to the healthcare link above. The 2019 federeal poverty guidelines were published 2/1/2019, while the 2018 federal poverty guidelines were published on 1/18/18, so the 2018 guidelines apply.
    
    subsidy_pov_level_ssi <- 0
    percent_of_poverty_min <- 0 # See table
    percent_of_poverty_max <- 0 # See table
    premium_cap_pct <- 0 # See table
    percent_of_poverty_ssi <- 0
    hlth_gross_income_m_ssi <- 0
    subsidy_pov_level_m_ssi <- subsidy_pov_levels[family_size + disability_parent1 + disability_parent2] # This  allows the FRS to reassess Medicaid eligibility when families have at least one family member receiving SSI, in states (like at least DC in 2017) that define the family unit potentially eligible for Medicaid as the members of a family who are not already eligibel for Medicaid due to SSI receipt. Whether or not this variable and subsequent variables related to health insurance that use the _ssi suffix are used for a specific state is determined by the hlth codes for each state in any given FRS year. 
    
    # 1.  Calculate Modified Adjusted Gross Income (MAGI) 
    hlth_gross_income <- earnings_mnth + interest_m
    
    # 2. Determine potential health care subsidy level 
    # Use subsidy_pov_level_m chart to find applicable subsidy_pov_level_m for family size.
    percent_of_poverty <- hlth_gross_income / subsidy_pov_level_m
    
    if (ssi_recd > 0) {
        hlth_gross_income_m_ssi <- hlth_gross_income_m
        if (disability_parent1 == 1) {
            hlth_gross_income_m_ssi <- pos_sub(hlth_gross_income_m_ssi, parent1_earnings)
        }
        if (disability_parent2 == 2) {
            hlth_gross_income_m_ssi <- pos_sub(hlth_gross_income_m_ssi, parent2_earnings)
        }
        percent_of_poverty_ssi <- hlth_gross_income_m_ssi / subsidy_pov_level_m_ssi
    }
    
    # Use premium_cap table to find applicable premium_cap_pct for 'percent_of_poverty'. This table actually should be derived from the IRS notices regarding employer plan affordability and calculating an individuals's premium tax credit, available for 2020 plans (for open enrollment beginning in November 2019) at https://www.irs.gov/pub/irs-drop/rp-19-29.pdf. This is an annually-issued document. This includes both the required employer contribution level for 2019 (9.86%) as well as the applicable percentages for calcluating the premium tax credit, which are the percentages at 100%, 133%, 150%, 200%, 250%, 300%, and 400%. IRS rule § 36B(b)(3)(A)(i) indicates that between these percentages, the applicable percentages increases linearly, on a sliding scale, allowing for the rest of these rows to be filled in based on this formula.
    
    # In 2015, when the ACA changes were introduced to the FRS, we planned on adding the calculations of premium_cap_pct to the SQL database. While that would still be ideal in an ideal world, hard-coding this every year based on the annual updates to the IRS codes around this works okay too, for the time being.
    
    poverty_percent_levels_IRS <- c(1, 1.33, 1.34, 1.35, 1.36, 1.37, 1.38, 1.39, 1.4, 1.41, 1.42, 1.43, 1.44, 1.45, 1.46, 1.47, 1.48, 1.49, 1.5, 1.51, 1.52, 1.53, 1.54, 1.55, 1.56, 1.57, 1.58, 1.59, 1.6, 1.61, 1.62, 1.63, 1.64, 1.65, 1.66, 1.67, 1.68, 1.69, 1.7, 1.71, 1.72, 1.73, 1.74, 1.75, 1.76, 1.77, 1.78, 1.79, 1.8, 1.81, 1.82, 1.83, 1.84, 1.85, 1.86, 1.87, 1.88, 1.89, 1.9, 1.91, 1.92, 1.93, 1.94, 1.95, 1.96, 1.97, 1.98, 1.99, 2, 2.01, 2.02, 2.03, 2.04, 2.05, 2.06, 2.07, 2.08, 2.09, 2.1, 2.11, 2.12, 2.13, 2.14, 2.15, 2.16, 2.17, 2.18, 2.19, 2.2, 2.21, 2.22, 2.23, 2.24, 2.25, 2.26, 2.27, 2.28, 2.29, 2.3, 2.31, 2.32, 2.33, 2.34, 2.35, 2.36, 2.37, 2.38, 2.39, 2.4, 2.41, 2.42, 2.43, 2.44, 2.45, 2.46, 2.47, 2.48, 2.49, 2.5, 2.51, 2.52, 2.53, 2.54, 2.55, 2.56, 2.57, 2.58, 2.59, 2.6, 2.61, 2.62, 2.63, 2.64, 2.65, 2.66, 2.67, 2.68, 2.69, 2.7, 2.71, 2.72, 2.73, 2.74, 2.75, 2.76, 2.77, 2.78, 2.79, 2.8, 2.81, 2.82, 2.83, 2.84, 2.85, 2.86, 2.87, 2.88, 2.89, 2.9, 2.91, 2.92, 2.93, 2.94, 2.95, 2.96, 2.97, 2.98, 2.99, 3, 4)
    premium_cap_percentages_IRS <- c(0, 0.0206, 0.0218, 0.023, 0.0242, 0.0254, 0.0267, 0.0279, 0.0291, 0.0303, 0.0315, 0.0327, 0.0339, 0.0351, 0.0364, 0.0376, 0.0388, 0.04, 0.0412, 0.0417, 0.0421, 0.0426, 0.0431, 0.0436, 0.044, 0.0445, 0.045, 0.0455, 0.0459, 0.0464, 0.0469, 0.0474, 0.0478, 0.0483, 0.0488, 0.0493, 0.0497, 0.0502, 0.0507, 0.0512, 0.0516, 0.0521, 0.0526, 0.0531, 0.0535, 0.054, 0.0545, 0.0549, 0.0554, 0.0559, 0.0564, 0.0568, 0.0573, 0.0578, 0.0583, 0.0587, 0.0592, 0.0597, 0.0602, 0.0606, 0.0611, 0.0616, 0.0621, 0.0625, 0.063, 0.0635, 0.064, 0.0644, 0.0649, 0.0653, 0.0656, 0.066, 0.0663, 0.0667, 0.0671, 0.0674, 0.0678, 0.0681, 0.0685, 0.0689, 0.0692, 0.0696, 0.0699, 0.0703, 0.0707, 0.071, 0.0714, 0.0717, 0.0721, 0.0725, 0.0728, 0.0732, 0.0735, 0.0739, 0.0743, 0.0746, 0.075, 0.0753, 0.0757, 0.0761, 0.0764, 0.0768, 0.0771, 0.0775, 0.0779, 0.0782, 0.0786, 0.0789, 0.0793, 0.0797, 0.08, 0.0804, 0.0807, 0.0811, 0.0815, 0.0818, 0.0822, 0.0825, 0.0829, 0.0832, 0.0835, 0.0838, 0.0841, 0.0844, 0.0847, 0.085, 0.0853, 0.0856, 0.0859, 0.0862, 0.0865, 0.0868, 0.0871, 0.0874, 0.0877, 0.088, 0.0883, 0.0886, 0.0889, 0.0892, 0.0895, 0.0898, 0.0901, 0.0904, 0.0906, 0.0909, 0.0912, 0.0915, 0.0918, 0.0921, 0.0924, 0.0927, 0.093, 0.0933, 0.0936, 0.0939, 0.0942, 0.0945, 0.0948, 0.0951, 0.0954, 0.0957, 0.096, 0.0963, 0.0966, 0.0969, 0.0972, 0.0975, 0.0978, 0.0978)
    
    IRS_table <- data.frame("Percent_of_poverty" = as.numeric(poverty_percent_levels_IRS), 
                            "Premium_Cap_Pcts" = as.numeric(premium_cap_percentages_IRS))
    
    IRS_poverty_rate <- max(poverty_percent_levels_IRS[premium_cap_percentages_IRS <= percent_of_poverty])
    premium_cap_pct <- IRS_table$"Premium_Cap_Pcts"[match(IRS_poverty_rate, IRS_table$"Percent_of_poverty")]
    
    private_max <- premium_cap_pct * hlth_gross_income_m * 12
    
    outputs_hash[[ 'max_income_pct_employer' ]] <<- max_income_pct_employer
    outputs_hash[[ 'magi_disregard' ]] <<- magi_disregard
    outputs_hash[[ 'sub_minimum' ]] <<- sub_minimum
    outputs_hash[[ 'sub_maximum' ]] <<- sub_maximum
    outputs_hash[[ 'hlth_gross_income_m' ]] <<- hlth_gross_income_m
    outputs_hash[[ 'percent_of_poverty' ]] <<- percent_of_poverty
    outputs_hash[[ 'private_max' ]] <<- private_max
    outputs_hash[[ 'percent_of_poverty_ssi' ]] <<- percent_of_poverty_ssi
    outputs_hash[[ 'medically_needy' ]] <<- medically_needy

}

# Helper rent function! (To accomodate possible rent override. Returns rent_cost_m.)
rent_module <- function(selected_town, children_count, input_home_cost, other_rental_cost) {
    rent <- 0
    num_children <- as.integer(children_count)
    if (is.null(other_rental_cost)) {
        other_rental_cost <- 0
    }
    if (is.null(selected_town)) {
        rent <- 0
    } else if (num_children == 1 | num_children == 2) {
        rent <- maine_localities$"fmr_2"[match(selected_town, maine_localities$"name")] * 12
    } else if (num_children == 3 | num_children == 4) {
        rent <- maine_localities$"fmr_3"[match(selected_town, maine_localities$"name")] * 12
    } else {
        rent <- maine_localities$"fmr_4"[match(selected_town, maine_localities$"name")] * 12
    }
    
    if (input_home_cost == "hud_rent") {
        return(as.numeric(rent))
    } else {
        return(as.numeric(other_rental_cost) * 12)
    }
}

#Section 8 Module, 2020.
section8_module <- function(earnings, sec8, child_age, interval, rent_cost_m, family_size, last_received_sec8, disability_parent1, disability_parent2, disability_work_expenses_m, interest, parent1_earnings, parent2_earnings, ssi_recd, tanf_recd, child_support_recd, max_housing_allowance_m, child_care_expenses, selected_town, health_expenses) {
    
    # Create outputs.
    rent_paid <- 0 # Annual rent paid by family: Tenant rent burden or full rent for families w/out subsidies
    rent_paid_m <- 0 # Monthly rent paid by family
    housing_recd <- 0 # Housing subsidy (Section 8 voucher) value, annual
    housing_subsidized <- 0 # a logical value indicating whether housing is subsidized
    
    # Variables used and created here.
    sec8_dependent_ded <- 480 # exemption per dependent
    sec8_dis_ded <- 400 # exemption for any disabled family member
    sec8_cc_ded_recd <- 0 # child care deduction
    sec8_gross_income <- 0 # gross income for determining tenant rent burden
    sec8_net_income <- 0 # adjusted income for determining tenant rent burden
    rent_preliminary <- 0 # preliminary rent [assuming (continuing) eligibility for vouchers]
    sec8_payment_standard <- 0 # HUD payment standard used to determine subsidy, based on Fair Market Rents.
    verylow_income_limit <- 0 #	very-low income limit used by HUD as a base to determine entrance eligibility
    low_income_limit <- 0 # low income limit used by HUD as a base to determine exit eligibility
    ami_adjustment <- c(0, 0.7, 0,8, 0.9, 1, 1.08, 1.16, 1.24)[family_size] # The family-size adjustment factors that HUD uses to determine income limits based on the 4-person base numbers. Source: see page 4 in https://www.huduser.gov/portal/datasets/il/il17/HUD-sec8-FY17.pdf 
    base_50_percent_ami <- 0
    base_80_percent_ami <- 0
    dis_asst_ded <- 0 # disability expenses deduction for determining net income
    med_expenses_ded <- 0 # medical expenses deduction for determining net income
    rent_difference <- 0 #2 019 addition, to account for voucher programs where renters can pay the difference between Section 8 standards and available rent costs.
    
    # Intro Note 1: The last_received_sec8 variable indicates the earnings level that family last had 
    # their rent subsidized by section 8 housing subsidies. It is assigned a value of 0 in the base program 
    # (frs.pl), and retains that value unless/until it is changed in this (the Section 8) module. If changed 
    # in the Section 8 module, it should keep the new value unless/until it is changed again in this same 
    # (Section 8) module again at a different earnings level. It is through this variable that we are able to 
    # disallow families no longer eligible for the program at one earnings level above entrance eligibility 
    # criteria from receiving section 8 once their earnings exceed entrance eligibility criteria, even if that 
    # earnings level is below the exit eligibility criteria but they qualify via a negative adjusted income shock 
    # (such as would happen if they lose child care subsidies).
    
    # Intro Note 2: It seems important to note that in the frs.pm program, rent_cost_m is (or seems to be) calculated 
    # either as (a) housing_override_amt, if the housing_override input flag is selected, or (b) rent, from the base tables, 
    # based on id (location) and number_children. It is therefore not necessarily equal to the fair market rent.
    
    # Intro Note 3: Because the 2015 FRS does not include VT or NY, I have commented out the code that refers solely to those states.
    
    if (sec8 != 1) {
        rent_paid <- rent_cost_m * 12
        rent_paid <- rent_paid / 12
        housing_recd <- 0
        
        # END
    } else {
        # Determine eligibility
        # Use "Locations" tab in the base table to determine fair market rent, used for the section 8 payment standard, based on year, state, residence, and number_children, labeling the associated value as sec8_payment_standard. Because we are shifting from  model in previous years that limited locations to a new approach of modeling many more residences in a single state, we have adjusted this SQL code to include lookups for 1-br, 2-br, 3-br, and 4-br FMRs based on family size, rather than building those different values in multiple rows for the same locality in SQL There may be a more elegant way to do this SQL call but this shoudl get the job done.
        
        # Using Google Sheet table to calculate FMR.
        num_children <- as.integer(length(child_age))
        if (num_children == 1 | num_children == 2) {
            sec8_payment_standard <- maine_localities$"fmr_2"[match(selected_town, maine_localities$"name")] * 12
        } else if (num_children == 3 | num_children == 4) {
            sec8_payment_standard <- maine_localities$"fmr_3"[match(selected_town, maine_localities$"name")] * 12
        } else {
            sec8_payment_standard <- maine_localities$"fmr_4"[match(selected_town, maine_localities$"name")] * 12
        }
        
        base_50_percent_ami = maine_localities$"base_50_percent_ami"[match(selected_town, maine_localities$"name")]
        base_80_percent_ami = maine_localities$"base_80_percent_ami"[match(selected_town, maine_localities$"name")]
        
        verylow_income_limit <- round((base_50_percent_ami * ami_adjustment)/50) * 50 # Rounding to nearest 50.
        low_income_limit <- round((base_80_percent_ami * ami_adjustment)/50) * 50
        
        # Compute gross income
        sec8_gross_income <- earnings + tanf_recd + child_support_recd + interest + ssi_recd
        
        # We first calculate rent for instances either when the user accepts the fair market rent (the FRS default value) or when the user-entered rent value is lower than fair market rate. (We must also be explicit in the main interface of the 2017 DC FRS that if users enter a rent higher than 175% of the fair market rent, they will not be eligible for Section 8). Otherwise, at $0 earnings, the family would be paying more than 40% of their earnings on rent they need to pay the landlord, since that will be the difference between the payment standard (the maximum subsidy) and the rent on the unit. We are assuming that this family, once having earnings of $0, still lives in an appropriate unit that would meet this federal standard. As far as including ssi income for eligibility determination, there is a memo from 2012 that indicates as such. 
        # 2019 adjustment: I think the above is too restrictive and may just be for place-based section 8, and not the voucher program. It also removes people from Section 8 eligibiltiy when the section 8 payment standards in certain areas, like Allegheny County, are lower than the fair market rents, which we use as defaults.
        
        if (rent_cost_m > sec8_payment_standard) {
            rent_difference <- rent_cost_m - sec8_payment_standard
            # END
        }
        
        # We then determine whether the family exceeds the low-income limit, which HUD uses to determine exit eligibility.
        
        if (sec8_gross_income > low_income_limit) {
            rent_paid <- rent_cost_m * 12
            rent_paid_m <- rent_paid / 12
            housing_recd <- 0
            # END
        } else if (last_received_sec8 > 0 & earnings - last_received_sec8 > interval & sec8_gross_income > verylow_income_limit) {
            rent_paid <- rent_cost_m * 12
            rent_paid_m <- rent_paid / 12
            housing_recd <- 0
        } else {
            # Calculate child care deduction
            sec8_cc_ded_recd <- min(child_care_expenses, sec8_gross_income)
            #Calculate disabled household allowance, disability assistance expenses, and medical expenses allowances for non-disabled populations.  There is only one household allowance, even if both parents are disabled. See 5-28 in HCV guidebook.
            if (disability_parent1 == 0 & disability_parent2 == 1) {
                sec8_dis_ded <- 0
                dis_asst_ded <- 0
                med_expenses_ded <- 0
            } else {
                sec8_dis_ded <- sec8_dis_ded
                
                #Calculate disability assistance expenses and medical expenses deductions for disabled households. While there are separate calculations for each, when a household qualifies for both (which would be the case if any parent is disabled), then there are specific instructions. 
                #The disability assistance expenses deduction is for unreimbursed expenses to cover any expenses that allow a family member to be employed, which we assume is disability_work_expenses_m. The allowance is capped at the amount of income made by the disabled individual. See HCV guidebook 5-30/33.
                
                if (disability_parent1 == 1) {
                    dis_asst_ded <- min(pos_sub(lengthCheck(disability_work_expenses_m) * 12, sec8_gross_income * 0.03), parent1_earnings)
                }
                # If there are any remaining disability_work_expenses, we apply them to the second parent if they also have a disability.
                if (disability_parent2 == 1) {
                    dis_asst_ded <- min(pos_sub(disability_work_expenses_m * 12, sec8_gross_income * 0.03), pparent2_earnings)
                }
                #Calculate medical expenses deduction. This deduction is only available to elderly or disabled households. 
                med_expenses_ded <- pos_sub(health_expenses, pos_sub(sec8_gross_income * 0.03, dis_asst_ded))
            }
            
            # Compute adjusted income
            sec8_net_income <- pos_sub(sec8_gross_income, (sec8_dependent_ded * num_children + sec8_cc_ded_recd + sec8_dis_ded + dis_asst_ded
                                                           + med_expenses_ded))
        }
        
        # 2. DETERMINE RENT
        # While we use the variable name rent_preliminary below, this is actually a calculation of the "Total Tenant Payment" (TTP) per HUD guidelines. When rent_cost_m is equal to the rent listed in the base tables, and that rent is based on the 50th percentile market rate  (fair market rent), we can use it as the “Payment Standard” that constitutes the maximum subsidy that HUD provides through the Housing Choice Voucher Program. 
        rent_preliminary <- max(0.3 * sec8_net_income, 0.1 * sec8_gross_income)
        rent_paid <- min(rent_preliminary + rent_difference, rent_cost_m * 12) #Note: rent_difference is added beginning in 2019 and beyond. This will allow better modeling of non-project based Section 8 vouchers.
        if (rent_paid < rent_cost_m * 12) {
            housing_subsidized = 1
        } else {
            housing_subsidized = 0
        }
        
        rent_paid_m = rent_paid / 12
        
        # 3. DETERMINE SUBSIDY VALUE.
        housing_recd <- rent_cost_m * 12 - rent_paid
        
        # Note that family has received section 8 at this income level
        if (housing_recd > 0) {
            last_received_sec8 = earnings
        }
        
    }
    
    outputs_hash[[ "rent_paid" ]] <<- rent_paid
    outputs_hash[[ "rent_paid_m" ]] <<- rent_paid_m
    outputs_hash[[ "housing_recd" ]] <<- housing_recd
    outputs_hash[[ "housing_subsidized" ]] <<- housing_subsidized
    outputs_hash[[ "last_received_sec8" ]] <<- last_received_sec8
    outputs_hash[[ "rent_difference" ]] <<- rent_difference
    
}

work_module <- function(earnings, family_structure, child_age, residence_size, parent2_max_work, other_cost_estimate, residence, parent1_employedhours, parent2_employedhours, parent_workhours_w, parent_otherhours_w, caregiver_workshifts_w, caregiver_maxworkweek, caregiver_maxshiftlength, caregiver_backtobackshifts, tanf_recd, shifts_parent1, shifts_parent2) {
    
    # Outputs created.
    parent_workhours_w <- parent_workhours_w # This variable has already been defined in parent_earnings, but may be revised based on TANF work requirements, below.
    parent1_employedhours_w <- parent1_employedhours # Just to make the code below easier
    parent2_employedhours_w <- parent2_employedhours # Just to make the code below easier
    shifts_parent1 <- shifts_parent1 # Might be revised here.
    shifts_parent2 <- shifts_parent2 # Might be revised here.
    transshifts_parent1 <- 0
    transshifts_parent2 <- 0
    multipleshifts_parent1 <- 0
    multipleshifts_parent2 <- 0
    parent_otherhours_w <- parent_otherhours_w
    caregiver_workshifts_w <- caregiver_workshifts_w
    caregiver_maxworkweek <- caregiver_maxworkweek
    caregiver_maxshiftlength <- caregiver_maxshiftlength
    caregiver_backtobackshifts <- caregiver_backtobackshifts
    trans_expenses <- 0 # Family transportation costs.
    
    # variables used in this script
    cost_per_mile <- 0.40 # IRS cost per mile (varies by year). This is up to date as of tax year 2019. Have used IRS mileage in the past, at https://www.irs.gov/newsroom/irs-issues-standard-mileage-rates-for-2019. Per KY request, using KY-specific rate of $0.40, per https://finance.ky.gov/Office%20of%20the%20Controller/ControllerDocuments/Mileage%20rate%20for%20website%2003%202020.pdf. 
    percent_nonsocial <- 0 # percent of miles driven for "nonsocial" purposes (used to determine parent1's transportation costs)
    percent_work <- 0 # percent of miles driven for "work" purposes (used to determine parent2's transportation costs)
    avg_miles_driven <- 0 # avg annual miles driven, based on size of place of residence
    parent1_transhours_w <- 0   # for transportation: number of hours per week that parent 1
                                # spends in paid employment, in addition to hours that a TANF-recipient
                                # parent spends in\ non-paid TANF work activities
    parent2_transhours_w <- 0   # same as above for second parent.  Note: in CT, TANF work requirements
                                # are low enough that  parent 2 never needs to participate in work activities
    parent1_transcost_full <- 0 # parent 1's transportation costs when parent 1 is working full time
    parent2_transcost_full <- 0 # ditto for second parent
    parent1_transcost <- 0 # parent's transportation costs when parent is working full time
    parent2_transcost <- 0 # ditto for second parent
    parent1_transdays_w <- 0    # number of days of transportation needed by parent 1, based on
                                # parent1_transhours_2 (for prorating "full-time" transportation cost)
    parent2_transdays_w <- 0 # ditto for second parent
    trans_type <- 0 # type of transportation used (car vs. public) based on place of residence
    public_trans_cost_d <- 0 # daily cost of commuting (ie, cost of round-trip fare)
    public_trans_cost_max <- 0 
    publictrans_cost_d_dis <- 0 # daily cost of commuting (ie, cost of round-trip fare) for #people with disabilities
    publictrans_cost_max_dis <- 0 # maximum cost of public transportation (ie, cost of 12 #monthly passes) for people with disabilities
    infantchildpresent <- 0 # indicates presence of at least one child in the household who is an infant.
    abawd_workreq <- 80
    
    # Calculate children_under6
    children_under6 <- 0
    for (i in values(child_age)) {
        if (i > 6) {
            children_under6 = children_under6 + 1
        } else if (i == 0) {
            infantchildpresent = 1 # indicates presence of children who are 0. 
        }
    }
    
    # 1. Determine number of hours the parents work (including time in TANF-required work activities) for the purposes of determining family child care needs and transportation costs
    if (child_number == 0) {
        #ABAWDs:
        parent_workhours_w <- 0 #There is no child care need in this family, since there are no children, so this variable -- which is the amount of potential child care need based on parent work schedules -- will also be 0.
        #Families without children cannot receive TANF, at least in KY in 2020 (possibly everywhere). So the below conditions, specific to families that receive TANF, will not apply to ABAWDs. But we need to adjust transhours based on attendance at SNAP E&T trainings in order to satisfy work requirements, when we model the provision to exclude SNAP recipients from benefits when they do not satisfy work requirements. We also check to see if the user has selected whether SNAP (E&T) trainings are available for satisfaction of work requirements:
        for (i in 1:family_structure) {
            
        }
    }
    
    
}

# Women, Infants, and Children, 2020.
wic_module <- function(wic, child_age, breastfeeding, earnings, fpl, interest, ssi_recd, hlth_cov_parent, hlth_cov_child, medically_needy, fsp_recd, tanf_recd, child_support_recd, parent1_age, parent2_age, disability_parent1, wic_elig_nslp, child_lunch_red) {
    
    # Outputs created
    # The DC 2017 model used estimates that included no methodology. The 2019 estimates come from  https://fns-prod.azureedge.net/sites/default/files/ops/WICFoodPackageCost2014.pdf, a much more rigorous study that included greater specificity. We have simplified the data from Table 3.2 for FRS users. This also assumes that a child just turned the age he is. (For example, an infant is modeled as being just born; a 4-yo is not modeled as ever turning 5. Mothers are either partially or fully breastfeeding, but not partially breastfeeding. Conceivably we could build greater specficity into the model.)
    
    wic_breastfeeding <- 61.85 #  WIC study Table 3.1, estimated monthly food package costs for Group VII, "Fully breastfeeding; partially (mostly) breastfeeding multiples; pregnant with multiples; pregnant who are also fully or partially (mostly) breastfeeding singleton infants."
    wic_notbreastfeeding <- 38.23 # WIC study Table 3.1, estimated monthly food package costs for Group VI, "Nonbreastfeeding postpartum; partially (minimally) breastfeeding (up to 6 months postpartum)."
    wic_breastfedinfants <- 70.44 # WIC study Table 3.1, estimated monthly food package costs for Group II-BF, fully breastfed infants 6-11.9 months. Note that the benefit is only for 6 months, since the infant is breastfed before then. 
    wic_formulafedinfants <- 144.51 # WIC study Table 3.1., estimated monthly food package costs for Group II-FF, fully formula-fed infants. This represents a weigthed average based on costs for formula fed infants 0-3.9 months (group II-FF-A), 4-5.9 months (group II-FF-B), and 6-11.9 months (group II-FF), to account for a monthly average.
    wic_1yochild <- 38.6 # WIC study Table 3.1, estimated monthly food package costs for Group IV-A, Children 1-1.9 years. 
    wic_2to4yochild <- 39.41 # WIC study Table 3.1, estimated monthly food package costs for Group IV-B, Children 1-1.9 years. 
    foodathomecpi2014 <- 241.33 # Needed to account for inflation from WIC study. Food at home CPI for all urban customers, 9/2014. Source: https://fred.stlouisfed.org/series/CUSR0000SAF11, from US BLS CPI estimates.
    foodathomecpi <- 242.572 # Food at home CPI for all urban customers, 1/2020.  Source: https://fred.stlouisfed.org/series/CUSR0000SAF11, from US BLS CPI estimates.  Possibly eventually this shoudl be aligned with when we determine food costs. Updated 2/2020.
    # our $wic_CVCmothers = 11;	# Commenting this out becaause it's factored into WIC study. May come in handy later. Vegetables and Fruit Cash-Value Check for new mothers. This assumes no multiples (twins, triplets, etc.); those mothers get $16.50/ month
    # our $wic_CVCchildren = 8;	# Commenting this out becaause it's factored into WIC study. May come in handy later. Vegetables and Fruit Cash-Value Check for young children per month.
    wic_inc_limit <- 1.85 # The income eligibility limit as a % of federal poverty guideline.
    wic_income <- 0 # countable WIC income per FNS guidelines
    wic_recd <- 0 #  Estimated monetary value of WIC
    
    # RECERTIFICATION NOTE: Recertification occurs once  every 6 months to a year, see https://www.fns.usda.gov/wic/who-gets-wic-and-how-apply. Also according to that list, there can be waiting periods if enough people apply to WIC.
    # POLICY NOTE REGARDING FARMER'S MARKET BENEFITS: In some areas, like DC, there are also farmer’s market nutrition programs (FMNP), see https://www.fns.usda.gov/fmnp/wic-farmers-market-nutrition-program-fmnp. Upon inquiry in 2017-18, DC DHS clarified that they are not interested in us modeling farmer's market benefits this year, or at least for the parameters of this project. We have retained this practice of not including farmer's market benefits in 2019.
    # POLICY NOTE: WIC is not an entitlement. https://www.fns.usda.gov/wic/about-wic-wic-glance. But coverage rate is fairly high, according to USDA study posted on website, at about 60 percent of eligible, and about 85 percent of women and infants eligible.
    # In DC, the state/jurisdiction we first used for an FRS WIC module, we found that WIC Policy & Procedure Number 8.007, page 18, makes it clear that administrators assume nutritional or medical conditions are met for all WIC applicants with children of eligible ages. Despite clear guidelines on how to fill out the dietary and nutritional assessments, it appears that when families apply for WIC, nearly all pregnant and postpardum mothers, and all children under 6, are eligible for the program when dietary eligibility guidelines are appropriately followed by the certified professional administrators (CPAs) at WIC offices. For the time being, we are then assuming that all dietary eligibility guidelines are met. See "Estimating Eligibility and Participation for the WIC Program: Final Report," (2002), Chapter 7, at https://www.ncbi.nlm.nih.gov/books/NBK221951/#ddd00086. 
    # Certain applicants can be determined income-eligible for WIC based on their participation in certain programs. These included individuals: 
    # * eligible to receive SNAP benefits, Medicaid, for Temporary Assistance for Needy Families (TANF, formerly known as AFDC, Aid to Families with Dependent Children),
    # * in which certain family members are eligible to receive Medicaid or TANF, or
    # * at State agency option, individuals that are eligible to participate in certain other State-administered programs.
    # In other words, any individuals in families that receive SNAP or TANF meet the income requirement for WIC, and, assuming (as above) that criteria for nutritional risk are met, also will be able to receive WIC. Further, discussion of WIC rules at https://www.ncbi.nlm.nih.gov/books/NBK223563/ indicate that mothers and young children eligible for Medicaid ("certain family members") are eligible for WIC. It's not immediately clear if any states offer additional state programs also confer categorical eligibilty, though, but it seems that for the most part, there isn't much flexibility. In DC, for example, individuals  qualify for WIC income test in DC if they “[m]eet income guidelines or medical risk for your family as listed below or are participating in Medicaid, DC Healthy Families [CHIP], School Lunch Program, Temporary Assistance for Needy Families (TANF), or the Food Stamp Program.” But that guidance may be more for administrators than for actualy policy formula, as the school lunch program has identical income critera as WIC and I believe DC's CHIP program was merged with Medicaid.
    # POLICY NOTE: States with SNAP/TANF BBCE also increase WIC eligibility, and states can also confer WIC to families making under Medicaid income limits. Each child on Medicaid adds more to WIC benefits. There has been some literature on this but have concluded that Medicaid expansions above WIC guidelines likely do not increase WIC takeup because above 185% of poverty, most parents are on employer insurance.
    
    # Determine eligibility for each family member based on DC DOH
    
    # For the mother, we first determine eligibility and then estimate WIC benefit based on the “approximate cost benefit” of the WIC food packages at https://doh.dc.gov/page/sample-food-packages. When we did this for DC in 2017, the numbers were much higher than the average monthly benefit per person calculations by US FNS at https://www.fns.usda.gov/pd/wic-program. Why was unclear -- presumably include cost savings based on competitive bidding by DC -- but the new way we're doing this is more exact:
    
    # 1: Check for WIC flag
    # WIC
    if (wic == 0) {
        wic_recd <- 0
    } else {
         # Determine countable income for determining WIC eligibility. Per https://www.fns.usda.gov/sites/default/files/2013-3-IncomeEligibilityGuidance.pdf.
        wic_income <- earnings + ssi_recd + interest + tanf_recd + child_support_recd
    }
    #browser()
    if ((0 %in% values(child_age)) & (wic_income / fpl <= wic_inc_limit | hlth_cov_parent == "Medicaid" | hlth_cov_parent == "Medicaid and private" & (parent1_age == 18 | disability_parent1 == 1) | fsp_recd > 0 | tanf_recd > 0)) {
        #https://chfs.ky.gov/agencies/dph/dmch/nsb/Documents/WIC%20Income%20Guidelines.pdf, which adheres to federal income levels but also indicates that individuals who receive Medicaid or SNAP are eligible. It says nothing about TANF eligibiltiy, but the certfication manual at https://chfs.ky.gov/agencies/dph/dmch/nsb/Documents/certimgnt.pdf makes it clear that individuals receiving TANF are eligible. LOOK AT ME: What if this family member is excluded from TANF becuase they are receiving SSI instead? Are they still "TANF eligible" and categorically eligible for WIC as a result? Are there other situations where one family member is on TANF and another is not? 
        if (breastfeeding == 1) {
            wic_recd <- wic_breastfeeding * 12
        } else {
            # Non-breastfeeding mothers are only eligible to receive up to 6 months of WIC.
            wic_recd <- wic_notbreastfeeding * 6
        }
    }
    
    # 2: Determine eligibility and benefit.
    num_children <- length(child_age)
    
    for (i in 1:num_children) {
        if (!is.null(child_age[[ toString(i) ]] & child_age[[ toString(i) ]] < 5)) {
            if (wic_income / fpl <= wic_inc_limit | fsp_recd > 0 | tanf_recd > 0 | hlth_cov_child[[ toString(i) ]] == "Medicaid" | 
                hlth_cov_child[[ toString(i) ]] == "Medicaid/CHIP" | wic_elig_nslp == 1 & child_lunch_red[[ toString(i) ]] > 0) {
                if (child_age[[ toString(i) ]] == 0) {
                    if (breastfeeding == 1) {
                        wic_recd <- wic_recd + wic_breastfedinfants * 6
                    } else {
                        wic_recd <- wic_recd + wic_formulafedinfants * 12
                    }
                } else if (child_age[[ toString(i) ]] == 1) {
                    wic_recd <- wic_recd + wic_formulafedinfants * 12
                } else {
                    wic_recd <- wic_recd + wic_2to4yochild * 12
                }
            }
        }
    }
    
    # Adjust for inflation since 2014.
    wic_recd <- wic_recd * foodathomecpi / foodathomecpi2014
    
    outputs_hash[[ "wic_recd" ]] <- wic_recd
    
}

# Federal Income Tax, 2020.
fedtax_module <- function(family_structure, child_age, cadc, ctc, disability_parent2, disability_personal_expenses_m, parent2_max_hours_w, earnings, interest, ssi_recd, tanf_recd, child_support_recd, child_care_expenses, afterschool_expenses, fsp_record, parent1_earnings, parent2_earnings, liheap_record, heap_recd) {
    
    children_13 <- sum(values(child_age) <= 13)
    children_17 <- sum(values(child_age) <= 17)
    child_number <- length(child_age)
    
    # Outputs created.
    federal_tax_gross <- 0 # gross federal taxes, before subtracting CADC and CTC
    federal_tax_credits <- 0 # [Fed Tax Credits]
    federal_tax <- 0 # annual federal tax liability, NOT including the CTC or the EITC
    cadc_record <- 0 # [Fed CADC] annual value of child and dependent tax care credit
                    # (cannot be less than pre-CADC tax liability; does not take into account CTC)
    cadc_base <- 0 # Qualified child care expenses for determining CADC credit
    ctc_nonref_recd <- 0 # annual child tax credit, non-refundable portion
                        # (cannot be less than pre-CTC tax liability; does take into account CADC)
    cadc_real_recd <- 0 # "real" value of CADC, given eligibility for child tax credit
                        # (i.e., this is what the value of the CADC would be if the Child Tax Credit were subtracted 
                        # from gross tax liability first)
    filing_status <- 0 # filing status
    federal_tax_income <- 0 # adjusted income for calculating taxes
    
    # Additional variables used within the module.
    cadc_max_claims <- 3000 # maximum child care expenses that can be claimed (per child, up to 2)
    ctc_per_child <- 2000 # 2019 max child tax credit (per child)
    ded_per_exempt <- 4050 # deduction amount per exemption #5/11: changed this to $4,050 to represent change in tax policy beginning in 2016.
    tax_rate1 <- 0.10 # 2019 tax rate for income bracket 1
    tax_rate2 <- 0.12 # 2019 tax rate for income bracket 2
    tax_rate3 <- 0.22 # 2019 tax rate for income bracket 3
    tax_rate4 <- 0.24 # 2019 tax rate for income bracket 4
    tax_rate5 <- 0.32 # 2019 tax rate for income bracket 5
    tax_rate6 <- 0.25 # 2019 tax rate for income bracket 6
    tax_rate7 <- 0.37 # 2019 tax rate for income bracket 7
    
    # Variables set.
    home <- 0 # parent(s) meet the test of keeping up a home (1|0)
    support <- 0 # parent(s) meet the test of supporting a child (1|0)
    
    # exempt_number <- 0 # number of exemptions family can claim. Excluded beginning in 2019 because 2017 tax reform (beginning in 2018 tax year) removed exemptions.
    
    standard_deduction <- 0 #standard_deduction
    max_taxrate1 <- 0 # max adjusted income for tax rate 1
    max_taxrate2 <- 0 # max adjusted income for tax rate 2
    max_taxrate3 <- 0 # max adjusted income for tax rate 3
    max_taxrate4 <- 0 # max adjusted income for tax rate 4
    max_taxrate5 <- 0 # max adjusted income for tax rate 5
    max_taxrate6 <- 0 # max adjusted income for tax rate 6
    
    federal_tax_cadc <- 0 # federal tax liability after subtracting cadc, but before ctc
    cadc_gross <- 0 # gross CADC amount (i.e., before comparing to tax liability)
    cadc_percentage <- 0
    ctc_max_income <- 0 # income limit for max child tax credit (varies by filing status)
    
    ctc_number <- 0 # number of children eligible for child tax credit
    ctc_potential <- 0 # max potential child tax credit family may be eligible for (ie, ctc_per_child * ctc_number)
    ctc_reduction <- 0 # reduction amount for filers w/income above ctc_max_income (line 7 in ctc worksheet)
    ctc_potential_red <- 0 # max potential child tax credit, after subtracting ctc_reduction
    
    cadc_dis <- 0 # the only values for cadc_dis would be 0 and 1 to indicate whether or not 2nd parent is disabled and care for the parents would qualify for cadc
    
    # 1. Determine filing status, number of exemptions, and number of children for child tax credit
    # (note: to be claimed for the child tax credit, a child must be claimed as a dependent)
    
    # (We are only including cash and near-cash benefits in our calculation of public support
    # for the home and dependent support tests -- this is in keeping with common practice among tax preparers.)
    
    # Note beginning in 2019: All children in the FRS are considered "qualifying children" for tax purposes because they earn no income of their own. According to the 1040 instructions, this means that they are a qualifying child on someone's tax return. For single parents, they are a qualifying child either on the custodial parent's return or noncustodial parent's return, but regardless they are still a "qualifying child." This  means that all single-parent families being modeled in the FRS - at least up to 2019 - qualify as heads of household. In order to claim head of household, the child has to be young enough (under 19, all cases in the FRS), staying in the same home as the parent (all cases), and not paying more than half of the expenses that they themselves incur. This last condition is not the same as a condition that the single parent pays more than half of the child's expenses, it only means that THE CHILD HIMSELF OR HERSELF DOES NOT PAY more than half of their expenses. Previous versions of the FRS code (possibly of IRS code) compared earnigns and income against child support and TANF or other payments, but this is not correct. Whether or not a parent paid more than half of a child's expenses may be important in determining whether a child is dependent, though, which is explored later.
    
    # The following code uses Table “Federal Income Taxes tables”, worksheet I to determine:
    # - Filing_status
    # - Exempt_number
    # After the determination of filing status, the following if-block replicate the “Federal Income Taxes tables”, worksheet II to determine:
    # Use Table “Federal Income Taxes tables”, worksheet II to determine:
    # - Standard_deduction
    # - Max_taxrate1
    # - Max_taxrate2
    # - Max_taxrate3
    # - Max_taxrate4
    # - Max_taxrate5
    # - Max_taxrate6
    # - CTC_max_income
    
    if (family_structure == 1)  {
        if (child_number >= 1) {
            filing_status <- "Head of Household"
        } else {
            filing_status <- "Single"
        }
    } else {
        filing_status <- "Married"
    }
    
    # determine standard deduction, max adjusted income, and income limit for child tax credit
    
    if (filing_status == "Head of Household") {
        standard_deduction <- 18650
        max_taxrate1 <- 14100
        max_taxrate2 <- 53700
        max_taxrate3 <- 85500
        max_taxrate4 <- 163300
        max_taxrate5 <- 207350
        max_taxrate6 <- 518400
        ctc_max_income <- 200000
    } else if (filing_status == "Married") {
        standard_deduction <- 24800
        max_taxrate1 <- 19750
        max_taxrate2 <- 80250
        max_taxrate3 <- 171050
        max_taxrate4 <- 326600
        max_taxrate5 <- 414700
        max_taxrate6 <- 622050
        ctc_max_income <- 400000
    } else if (filing_status == "Single") {
        standard_deduction <- 12400
        max_taxrate1 <- 9875
        max_taxrate2 <- 40125
        max_taxrate3 <- 85525
        max_taxrate4 <- 163300
        max_taxrate5 <- 207350
        max_taxrate6 <- 518400
        ctc_max_income <- 200000
    }
    
    # 2. Determine gross federal tax (before CADC and Child Tax Credit)
    
    federal_tax_income <- earnings + interest - standard_deduction #Current formula, since 2017 tax reform removed exemptions.
    
    if (federal_tax_income <= 0) {
        federal_tax_income <- 0
        federal_tax_gross <- 0
    } else {
        if (federal_tax_income <= max_taxrate1) {
            federal_tax_gross <- tax_rate1 * federal_tax_income
        } else if (federal_tax_income <= max_taxrate2) {
            federal_tax_gross <- tax_rate2 * (federal_tax_income - max_taxrate1) + tax_rate1 * max_taxrate1
        } else if (federal_tax_income <= max_taxrate3) {
            federal_tax_gross <- tax_rate3 * (federal_tax_income - max_taxrate2) + tax_rate2 * (federal_tax_income - max_taxrate1) + tax_rate1 * max_taxrate1
        } else if (federal_tax_income <= max_taxrate4) {
            federal_tax_gross <- tax_rate4 * (federal_income_tax - max_taxrate3) + tax_rate3 * (federal_tax_income - max_taxrate2) + tax_rate2 * (federal_tax_income - max_taxrate1) + tax_rate1 * max_taxrate1
        } else if (federal_tax_income <= max_taxrate5) {
            federal_tax_gross <- tax_rate5 * (federal_income_tax - max_taxrate4) + tax_rate4 * (federal_income_tax - max_taxrate3) + tax_rate3 * (federal_tax_income - max_taxrate2) + tax_rate2 * (federal_tax_income - max_taxrate1) + tax_rate1 * max_taxrate1
        } else if (federal_tax_income <- max_taxrate6) {
            federal_tax_gross <- tax_rate6 * (federal_income_tax - max_taxrate5) + tax_rate5 * (federal_income_tax - max_taxrate4) + tax_rate4 * (federal_income_tax - max_taxrate3) + tax_rate3 * (federal_tax_income - max_taxrate2) + tax_rate2 * (federal_tax_income - max_taxrate1) + tax_rate1 * max_taxrate1
        } else if (federal_tax_income > max_taxrate6) {
            federal_tax_gross <- tax_rate7 * (federal_income_tax - max_taxrate6) + tax_rate6 * (federal_income_tax - max_taxrate5) + tax_rate5 * (federal_income_tax - max_taxrate4) + tax_rate4 * (federal_income_tax - max_taxrate3) + tax_rate3 * (federal_tax_income - max_taxrate2) + tax_rate2 * (federal_tax_income - max_taxrate1) + tax_rate1 * max_taxrate1
        }
    }
    
    # 3. Determine CADC and “final” federal tax liability (not including EITC or CTC)
    
    if (isTRUE(cadc != 1)) {
        cadc_recd <- 0
    } else {
        # Note: Separate from the question of whether a filer can file as head of household or single, children and other relatives int the home may be classified as dependents for the tax filer to claim certain tax credits. In the FRS model, all children listed in the household for whom the parent(s) pay half the support for qualify as a dependent. In addition, a spouse who has a disability that is severe enough that they cannot work and possibly require expenses for proper care can also be classified as a dependent.
        
        if (family_structure == 1) {
            cadc_base <- min(child_care_expenses + afterschool_expenses, children_13 * cadc_max_claims, 2 * cadc_max_claims, earnings)
        } else {
            cadc_base <- min(child_care_expenses + afterschool_expenses, children_13 * cadc_max_claims, 2 * cadc_max_claims, parent2_earnings,
                             parent1_earnings)
            if (isTRUE(disability_parent2 == 1 & parent2_max_hours_w == 0)) {
                cadc_dis <- 1
                cadc_base <- min(child_care_expenses + afterschool_expenses + 12 * disability_personal_expenses_m, (children_13 + cadc_dis) *
                                     cadc_max_claims, 2 * cadc_max_claims, parent1_earnings)
            }
            
            #SH created a “parent2_incapacitated” variable in the TANF code, with the value of 0 (not incapacitated) or 1 (incapacitated). The above follows the same determination. Parent2 is incapacitated because of disability, i.e. when disability_parent2 = a and parent2_max_hours_w and parent2_max_hours_w=0, since that means that parent2 is both disabled and will never be earning income. 
            #if the 2nd parent is disabled and not working, we assume that you pay for someone to provide care for #your disabled spouse and their care expenses qualify for cadc. We need to ensure that the user knows #this when entering the amount into disability_personal_expenses. 
            # In addition, previous iterations of the code were based on old parent_earnings calculations wherein which the second parent’s earnings would never be higher than the first parent’s. Since the new 2017 methods allow for parent 2 to earn more than parent 1, we include both parent’s earnings in the formulas.
            
        }
        
        # determine cadc_percentage. (These are available at https://www.irs.gov/pub/irs-pdf/p503.pdf for 2018 and seemingly beyond.)
        
        fed_incomes <- seq(15000, 43000, 2000)
        cadc_percentages <- seq(0.35, 0.20, -0.01)
        
        if (earnings + interest < 15000) {
            cadc_percentage <- 0.35
        } else {
            cadc_percentage <- cadc_percentages[match(max(fed_incomes[fed_incomes <= (earnings + interest)]), fed_incomes)]
        }
        
        cadc_gross <- cadc_percentage * cadc_base
        cadc_recd <- min(federal_tax_gross, cadc_gross)
        
        federal_tax_cadc <- federal_tax_gross - cadc_recd
    }
    
    federal_tax_cadc <- federal_tax_gross - cadc_recd
    federal_tax <- federal_tax_cadc
    
    # 4 Determine ctc_nonref_recd
    # Refer to CTC_max_income according to filing status from federal income taxes tables 2017. Use child tax credit worksheet in instructions for form 1040.
    
    if (ctc == 1) {
        ctc_potential <- ctc_per_child * children_17
        if (earnings + interest <= ctc_max_income) {
            ctc_reduction <- 0
        } else {
            ctc_reduction <- 0.05 * 1000 * ceil(earnings + interest - ctc_max_income)/1000
        }
        ctc_reduction_red <- ctc_potential - ctc_reduction
        if (ctc_potential_red <= 0) {
            ctc_nonref_recd <- 0
        } else {
            ctc_nonref_recd <- min(federal_tax_cadc, ctc_potential_red)
        }
    } else {
        ctc_nonref_recd <- 0
    }
    
    # determine federal tax liability (including all nonrefundable credits), and real value of CADC
    # TODO NIP
    
    cadc_real_recd <- min(cadc_gross, pos_sub(federal_tax_gross, ctc_potential_red))
    
    # Handle outputs.
    
    outputs_hash[[ 'federal_tax' ]] <- federal_tax
    outputs_hash[[ 'federal_tax_gross' ]] <- federal_tax_gross
    outputs_hash[[ 'cadc_recd' ]] <- cadc_recd
    outputs_hash[[ 'cadc_percentage' ]] <- cadc_percentage
    outputs_hash[[ 'ctc_nonref_recd' ]] <- ctc_nonref_recd
    outputs_hash[[ 'cadc_real_recd' ]] <- cadc_real_recd
    outputs_hash[[ 'filing_status' ]] <- filing_status
    outputs_hash[[ 'home' ]] <- home
    outputs_hash[[ 'support' ]] <- support
    outputs_hash[[ 'ctc_potential' ]] <- ctc_potential
    outputs_hash[[ 'ctc_reduction' ]] <- ctc_reduction
    outputs_hash[[ 'ctc_potential_red' ]] <- ctc_potential_red
    outputs_hash[[ 'federal_tax_cadc' ]] <- federal_tax_cadc
    outputs_hash[[ 'cadc_gross' ]] <- cadc_gross
    outputs_hash[[ 'federal_tax_income' ]] <- federal_tax_income
    outputs_hash[[ 'federal_tax_credits' ]] <- federal_tax_credits
    outputs_hash[[ 'cadc_base' ]] <- cadc_base

}

eitc_module <- function(eitc, earnings, child_age, family_structure, interest) {
    
    child_number <- length(child_age)
    
    # Outputs create.
    eitc_recd <- 0
    
    # Variables used in this module.
    eitc_phasein_rate <- 0 # eitc phase-in rate 
    eitc_plateau_start <- 0 #earned income amount
    eitc_plateau_end <- 0 #threshold phaseout amount
    eitc_max_value <- 0
    eitc_phaseout_rate <- 0
    eitc_income_limit <- 0 #completed phaseout amount
    
    #changes for 2017 . We are adding interest into earnings since the 2016 instructions for 1040 instruct  using the number from line 38 to estimate EITC eligibility which is earnings + interest. Prior versions did not include interest. We do not include investment income anywhere in the FRS, and thus, exclude it here as well.
    
    if (eitc == 0) {
        eitc_recd <- 0
    } else {
        # Use EITC table to determine (based on family_structure and child_number):
        if (child_number == 0) {
            eitc_phasein_rate <- 0.0765
            eitc_plateau_start <- 7030
            eitc_max_value <- 538
            eitc_phaseout_rate <- 0.0765
            if (family_structure == 1) {
                eitc_plateau_end <- 8790
                eitc_income_limit <- 15820
            } else {
                eitc_plateau_end <- 14680
                eitc_income_limit <- 21710
            }
        } else if (child_number == 1) {
            eitc_phasein_rate <- 0.34
            eitc_plateau_start <- 10540
            eitc_max_value <- 3584
            eitc_phaseout_rate <- 0.1598
            if (family_structure == 1) {
                eitc_plateau_end <- 19330
                eitc_income_limit <- 41756
            } else {
                eitc_plateau_end <- 25220
                eitc_income_limit <- 47646
            }
        } else if (child_number == 2) {
            eitc_phasein_rate <- 0.4
            eitc_plateau_start <- 14800
            eitc_max_value <- 5920
            eitc_phaseout_rate <- 0.2106
            if (family_structure == 1) {
                eitc_plateau_end <- 19330
                eitc_income_limit <- 47440
            } else {
                eitc_plateau_end <- 25220
                eitc_income_limit <- 53330
            }
        } else if (child_number >= 3) {
            eitc_phasein_rate <- 0.45
            eitc_plateau_start <- 14800
            eitc_max_value <- 6660
            eitc_phaseout_rate <- 0.2106
            if (family_structure == 1) {
                eitc_plateau_end <- 19330
                eitc_income_limit <- 50954
            } else {
                eitc_plateau_end <- 25220
                eitc_income_limit <- 56884
            }
        }
        if (earnings + interest >= eitc_income_limit) {
            eitc_recd <- 0
        } else if (earnings + interest < eitc_plateau_start) {
            eitc_recd <- eitc_phasein_rate * (earnings + interest)
        } else if (earnings + interest >= eitc_plateau_start & earnings + interest < eitc_plateau_end) {
            eitc_recd <- eitc_max_value
        } else if (earnings + interest >= eitc_plateau_end & earnings + interest < eitc_income_limit) {
            eitc_recd <- eitc_phaseout_rate * (eitc_income_limit - earnings - interest)
        }
        eitc_recd <- round(eitc_recd, 0)
    }
    
    outputs_hash[[ "eitc_recd" ]] <<- eitc_recd
    
}

# Payroll, 2020.
payroll_module <- function(parent1_earnings, parent2_earnings) {
    
    # Outputs created.
    payroll_tax <- 0
    parent1_payroll_tax <- 0
    parent2_payroll_tax <- 0
    
    # Variables used in this module.
    social_sec_rate <- 0.062 # social security tax rate
    medicare_rate <- 0.0145 # medicare rate
    ssec_income_limit <- 137700 # social security  wage base limit, the maximum wage subject to the tax for the year.
    add_medicare_tax <- 0.009 #additional Medicare tax to those earning more than $200,000. 
    add_medicare_inc <- 200000 #wage ceiling before additional Medicare tax kicks in.
    
    # Additional variables to be calculated.
    social_sec_tax_parent1 <- 0 # social security tax amount (parent 1)
    social_sec_tax_parent2 <- 0 # social security tax amount (parent 2)
    medicare_tax_parent1 <- 0 # medicare tax amount (parent 1)
    medicare_tax_parent2 <- 0 # medicare tax amount (parent 2)
    add_medicare_tax_parent1 <- 0 #additional medicare tax amount (parent 1)
    add_medicare_tax_parent2 <- 0 #additional medicare tax amount (parent 2)
    
    social_sec_tax_parent1 <- social_sec_rate * min(parent1_earnings, ssec_income_limit)
    social_sec_tax_parent2 <- social_sec_rate * min(parent2_earnings, ssec_income_limit)
    
    medicare_tax_parent1 <- medicare_rate * parent1_earnings
    medicare_tax_parent2 <- medicare_rate * parent2_earnings
    
    if (isTRUE(parent1_earnings > add_medicare_inc)) {
        add_medicare_tax_parent1 <- add_medicare_tax * (parent1_earnings - add_medicare_inc)
    } else {
        add_medicare_tax_parent1 <- 0
    }
    
    if (isTRUE(parent2_earnings > add_medicare_inc)) {
        add_medicare_tax_parent2 <- add_medicare_tax * (parent2_earnings - add_medicare_inc)
    } else {
        add_medicare_tax_parent2 <- 0
    }
    
    parent1_payroll_tax <- social_sec_tax_parent1 + medicare_tax_parent1 + add_medicare_tax_parent1
    parent2_payroll_tax <- social_sec_tax_parent2 + medicare_tax_parent2 + add_medicare_tax_parent2
    
    payroll_tax <- parent1_payroll_tax + parent2_payroll_tax
    
    outputs_hash[[ "payroll_tax" ]] <<- payroll_tax
    outputs_hash[[ "parent1_payroll_tax" ]] <<- parent1_payroll_tax
    outputs_hash[[ "parent2_payroll_tax" ]] <<- parent2_payroll_tax
    
}

# Child tax credit, 2020.
ctc_module <- function(earnings, child_age, family_structure, ctc, ctc_reduction, ctc_potential_red, federal_tax_cadc, ctc_nonref_recd, payroll_tax, eitc_recd, cadc_recd) {
    
    children_under17 <- sum(values(child_age) <= 17)
        
    # Create outputs.
    ctc_total_recd <- 0
    federal_tax_credits <- 0
    
    # Other variables used here.
    ctc_add_threshold <- 2500 # earnings threshold for the refundable child tax credit, as indicated on form 8812. (Earnings threshold above which the family's ACTC begins to increase by 15%)
    actc_per_child <- 1400 # The maximum additional (refundable) child tax credit per child.
    ctc_add_line3 <- 0 # line 3 on form 8812: excess child tax credit (ctc_potential - ctc_nonref_recd)
    ctc_add_line4 <- 0 # line 4 on form 8812.
    ctc_add_line5 <- 0 # line 5 on form 8812.
    ctc_add_line8 <- 0 # line 8 (formerly line 6, prior to 2018) on form 8812: 15% of earnings above ctc threshold (earnings - ctc_add_threshold) 
    ctc_add_line14 <- 0 # line 14 (formerly line 12, prior to 2018) on form 8812: greater of line 8 (15% of earnings above ctc threshold) OR line 13 (payroll_tax - EITC)
    ctc_additional_recd <- 0 # additional annual child tax credit (i.e., refundable portion)
    
    # 1. DETERMINE VALUE OF ADDITIONAL CTC
    if (ctc == 1) {
        ctc_additional_recd <- 0
        # SKIP TO STEP 2
    } else {
        if (ctc_potential_red <= 0) {
            ctc_additional_recd <- 0
            # SKIP TO STEP 2
        } else {
            ctc_add_line3 <- ctc_potential_red - ctc_nonref_recd
            ctc_add_line4 <- actc_per_child * children_under17
            ctc_add_line5 <- min(ctc_add_line3, ctc_add_line4)
            ctc_add_line8 <- 0.15 * pos_sub(earnings, ctc_add_threshold)
            
            if (ctc_add_line3 == 0) {
                ctc_additional_recd <- 0
                # SKIP TO STEP 2
            } else {
                if (children_under17 < 3) {
                    if (ctc_add_line8 == 0) {
                        ctc_additional_recd <- 0
                        # SKIP TO STEP 2
                    } else if (ctc_add_line8 > 0) {
                        ctc_additional_recd <- min(ctc_add_line5, ctc_add_line8)
                    }
                } else {
                    if (ctc_add_line8 >= ctc_add_line5) {
                        ctc_additional_recd <- ctc_add_line5
                    } else {
                        ctc_add_line14 <- max(ctc_add_line8, payroll_tax - eitc_recd)
                        ctc_additional_recd <- min(ctc_add_line14, ctc_add_line5)
                    }
                }
            }
        }
    }
    
    #STEP 2: CALCULATE THE TOTAL CHILD TAX CREDIT (REFUNDABLE AND NONREFUNDABLE) AND TOTAL FEDERAL TAX CREDITS. 
    ctc_total_recd <- ctc_additional_recd + ctc_nonref_recd
    federal_tax_credits <- eitc_recd + ctc_total_recd + cadc_recd
    
    #This is the last federal tax code. Because state credits are calculated after federal tax credits, we define the tax_before_credits and tax_after_credits variables in the state tax codes modules. So even if a state has no taxes, they should have a state tax module indicating as much and defining these variables. 
    
    #browser()
    outputs_hash[[ "ctc_additional_recd" ]] <<- ctc_additional_recd
    outputs_hash[[ "ctc_total_recd" ]] <<- ctc_total_recd
    outputs_hash[[ "federal_tax_credits" ]] <<- federal_tax_credits
}

# State and local tax module, Maine 2020.
stateandlocaltax_module <- function(earnings, heat_in_rent, disability_personal_expenses_m, state_cadc, state_eitc, state_dec, state_ptfc, state_stfc, parent1_incapacitated, parent2_incapacitated, family_structure, child_number, interest, cadc_recd, cadc_percentage, filing_status, federal_tax_gross, eitc_recd, federal_tax_credits, child_care_expenses, child_care_expenses_regular, child_care_expenses_step4, rent_paid, energy_cost) {
    
    # Checking inputs.
    family_structure <- as.integer(family_structure)
    
    # Outputs created.
    state_tax <- 0
    county_tax <- 0
    tax_before_credits <- 0
    tax_after_credits <- 0
    
    # Additional variables used in this module.
    state_filing_status <- 0 # filing status for ME
    state_gross_income <- 0
    state_tax_gross <- 0 # ME tax liability
    state_tax_income <- 0 # ME taxable income (earnings + interest)
    state_tax_rate1 <- .058 # state tax rate for incomes under first tax threshold
    state_tax_rate2 <- .0675 # state tax rate for incomes under second tax threshold
    state_tax_rate3 <- .0715
    state_standard_deduction <- 0 #Maine's standard deduction depends on income.
    state_exemption <- 4300 #Amount of the exemption from state tax, 2020
    state_exemption_amt <- 0 #Amount of the exemption from state tax
    dep_exempt_credit_potential <- 0
    dep_credit_max_perchild <- 300
    dep_exempt_max_thresh <- 200000 #The gross income at which the dependent exemption credit starts declining.
    dep_exempt_phaseout_rate <- .0075 #The marginal rate of decline on the dependent exemption tax credit for incomes above the threhold for receiving the highest credit amount.
    regular_fed_cadc_portion <- 0
    step4_fed_cadc_portion <- 0
    state_cadc_potential <- 0
    state_cadc_refundable <- 0
    state_cadc_nonrefundable <- 0
    refundable_cadc_max <- 500 # Maximum refundable child care credit.
    state_adultcare_credit_potential <- 0
    state_adultcare_credit_refundable <- 0
    state_adultcare_credit_nonrefundable <- 0
    refundable_adultcare_credit_max <- 500 # Maximum refundable adult care credit.
    state_cadc_pct_regular <- .25 #State CDCTC percent of federal credit for child care in settings considered "regular"
    state_adultcare_pct <- .25 #Separate state % of federal cdctc for adult care.
    regular_cc_costs_percent <- 0
    step4_cc_costs_percent <- 0
    state_cadc_pct_step4 <- .5 #State CDCTC percent of federal credit for child care in settings considered "regular"
    state_pftc_max_credit_nonelderly <- 750 # Maximum allowable PTFC amount
    state_pftc_util_portion_est <- .15 #How much the state is estimating utilty costs would be for people whose rent bills include utilities.
    state_ptfc_rent_portion <-.15 #The maximum portion of a filer's rent covered by the PTFC benefit base.
    state_ptfc_phaseout_rate <- .06 #The phase-out rate of the PTFC as income rises above the highest income that generates the maximum allowable PTFC amount.
    state_cadc_recd <- 0
    state_tax_credits <- 0
    limited_deduction_min <- 0
    max_limited_deduction <- 0
    limited_deduction_percent <- 0
    total_refundable_credits <- 0
    income_above_limited_deduction_min <- 0
    state_eitc_recd <- 0
    total_nonrefundable_credits <- 0
    state_tax_net<- 0
    state_ptfc_applicable_rent <- 0
    state_ptfc_rent_base <- 0
    state_ptfc_max_base <- 0
    state_ptfc_benefit_base <- 0
    state_ptfc_recd <- 0
    state_ptfc_benefit_base_reduced <- 0
    state_stfc_recd <- 0
    
    #There are no local or county income taxes in Maine. See https://taxfoundation.org/local-income-taxes-city-and-county-level-income-and-wage-taxes-continue-wane/.
    
    # Determine state filing status. Per Maine 2019 income tax instructions, this is the same as the federal filing status.
    state_filing_status <- filing_status
    
    # Determine thresholds and tax bases based on filing status, per 2020 rate schedules. We are using the 2019 tax forms as a guide, but the 2020 updates as published where possible.
    
    if (state_filing_status == "Single") {
        state_tax_threshold1 <- 22000
        state_tax_base1 <- 1288
        state_tax_threshold2 <- 52600
        state_tax_base2 <- 3340
    } else if (state_filing_status == "Head of Household") {
        state_tax_threshold1 <- 33000
        state_tax_base1 <- 1931
        state_tax_threshold2 <- 78900
        state_tax_base2 <- 5009
    } else { # for married couples
        state_tax_threshold1 <- 44450
        state_tax_base1 <- 2578
        state_tax_threshold2 <- 105200
        state_tax_base2 <- 6679
    }
    
    # determine state tax liability
    state_gross_income <- earnings + interest #This is the same formula as the federal gross income (AGI). We could also create a variable in the federal code for federal gross income; right now it includes that calculation inside the calculation for federal taxable income, but is not a stand-alone variable.
    
    # The following is based on an adjustment to the standard deduction made on higher income earners, based on instructions for line 17 of the 2019  Maine 1040 general instructions. It's unclear what these numbers are based on, if they tie into the federal codes or not. They are not updated in the 2020 income tax update:
    if (state_filing_status == "Single") {
        limited_deduction <- 81450
        max_limited_deduction <- 75000
    } else if (state_filing_status == "Head of Household") {
        limited_deduction_min <- 122200
        max_limited_deduction <- 112500
    } else { # for married couples
        limited_deduction_min <- 162950
        max_limited_deduction <- 150000
    }
    
    # Calculate state exemption amounts. In Maine, these track with whether an individual is single/head of household or married filing jointly.
    state_exemption_amt <- family_structure * state_exemption
    
    state_tax_income <- pos_sub(state_gross_income, state_standard_deduction + state_exemption_amt)
    
    if (state_tax_income < state_tax_threshold1) {
        state_tax_gross <- state_tax_rate1 * state_tax_income
    } else if (state_tax_income < state_tax_threshold2) {
        state_tax_gross <- state_tax_base1 + state_tax_rate2 * (state_tax_income - state_tax_threshold1)
    } else {
        state_tax_gross <- state_tax_base2 + state_tax_rate3 * (state_tax_income - state_tax_threshold2)
    }
    
    #TAX CREDITS:
    
    #Dependent Exemption Credit: 
    #As detailed in the worksheet in Schedule A, this gradually decilines at gross incomes exceeding $200,000. While the tool will likely rarely invoke tax calculations at these high incomes, we are keeping it in here for now to ensure accuracy. The below formula is  simplified version of the worksheet instructions. 
    
    if (state_dec == 1) {
         dep_exempt_credit_potential <- pos_sub(child_number * dep_credit_max_perchild, pos_sub(state_gross_income - dep_exempt_max_thresh) 
                                                * dep_exempt_phaseout_rate)
    }
    
    # Child care credit (refundable and nonrefundable):
    
    if (state_cadc == 1) {
        #NOTE: NEED TO DISTINGUISH BETWEEN STEP 4 ('QUALITY') CHILD CARE COSTS AND 'REGULAR' CHILD CARE COSTS IN MAINE'S CHILD CARE CODE. IN ADDITIONAL TO THE REGULAR VARIABLES, THIS SHOULD PRODUCE child_care_expenses_regular AND child_care_expenses_step4 VARIRABLES.
        regular_cc_costs_percent <- child_care_rexpenses_regular / child_care_expenses
        step4_cc_costs_percent <- child_care_expenses_step4 / child_care_expenses
        regular_fed_cadc_portion <- regular_cc_costs_percent * cadc_recd
        step4_fed_cadc_portion <- step4_cc_costs_percent * cadc_recd
        state_cadc_potential <- state_cadc_pct_regular * regular_fed_cadc_portion + state_cadc_pct_step4 * step4_fed_cadc_portion
        state_cadc_refundable <- min(state_cadc_potential, refundable_cadc_max)
        state_cadc_nonrefundable <- pos_sub(state_cadc_potential, state_cadc_refundable)
    
    # Adult dependent care credit
    # NOTE: WE NEED TO MAKE SURE TO HAVE The parent#_incapacitated variables as inputs. I believe right now they're outputs or in various codes, but we need to remove the assumptions those are built on and just ask people if any adult in the house has an incapacitating disability.
        if (family_structure == 2 & (parent1_incapacitated == 1 | parent2_incapacitated == 1)) {
            state_adultcare_credit_potential <- disability_personal_expenses_m * cadc_percentage * state_adultcare_pct
            state_adultcare_credit_refundable <- min(state_adultcare_credit_potential, refundable_adultcare_credit_max)
            state_adultcare_credit_nonrefundable <- pos_sub(state_adultcare_credit_potential, state_adultcare_credit_refundable)
        }
    }
    
    # EITC - for full-year residents, this is a completely refundable credit.
    if (state_eitc == 1) {
        state_eitc_recd <- 0.05 * eitc_recd
    }
    
    # Totals:
    total_nonrefundable_credits <- min(state_tax_gross, dep_exempt_credit_potential + state_cadc_nonrefundable + 
                                           state_adultcare_credit_nonrefundable)
    
    state_tax_net <- state_tax_gross - total_nonrefundable_credits
    
    #ADDITIONAL REFUNDABLE CREDITS
    
    #Somewhat counterintuitively given their names, you do not have to pay property tax to receive the property tax fairness credit, and the Sales Tax Fairness Credit is not at all contingent on how much you pay in sales tax.
    
    #Property Tax Fairness Credit (Schedule PTFC/STFC and instructions)
    # It's notable that Maine is one of the few states that offer a tax credit based on rent paid.
    
    #Schedule PTFC/STFC begins with income limits for the PTFC. We can ignore these limits, however, because that criteria is built into the the maximum credit allowable; there is no mathematical way a filer can receive the credit with incomes above the income limits.
    
    if (state_ptfc == 1) {
        if (heat_in_rent == 1) {
            state_ptfc_applicable_rent <- rent_paid - state_pftc_util_portion_est * rent_paid # Typo?
        } else {
            state_pftc_applicable_rent <- pos_sub(rent_paid, 12 * energy_cost) #We multiply energy costs (an output in the fsp (SNAP) module by 12 to get annual utility costs.
        }
        state_ptfc_rent_base <- state_ptfc_rent_portion * state_ptfc_applicable_rent
        if (state_filing_status == "Single") {
            state_ptfc_max_base <- 2650
        } else if (state_filing_status == "Head of Household") {
            if (child_numer <= 1) {
                state_ptfc_max_base <- 2650
            } else {
                state_ptfc_max_base <- 3300
            }
        } else { # for "Married". Does not seem like this credit is offered to married filing separately.
            if (child_number == 0) {
                state_ptfc_max_base <- 2650
            } else {
                state_ptfc_max_base <- 3300
            }
        }
        state_ptfc_benefit_base <- min(state_ptfc_rent_base, state_ptfc_benefit_base_max_base)
        if (state_ptfc_phaseout_rate * state_gross_income > state_ptfc_benefit_base) {
            state_ptfc_recd <- 0
        } else {
            state_ptfc_benefit_base_reduced <- state_ptfc_benefit_base - state_ptfc_phaseout_rate * state_gross_income
            state_ptfc_recd <- min(state_ptfc_benefit_base_reduced, state_pftc_max_credit_nonelderly)
        }
    }
    
    #Sales Tax Fairness Credit:
    if (state_stfc == 1) {
        if (state_filing_status == "Single") {
            income_levels <- c(20750, 21250, 21750, 22250, 22750, 23250, 23750, 24250, 24750, 25750, 26250, 26750)
            state_stfc_recd_levels <- c(125, 115, 105, 95, 85, 75, 65, 55, 45, 35, 25, 15, 5)
            if (state_gross_income <= 20750) {
                state_stfc_recd <- 125
            } else if (state_gross_income > 20750 & state_gross_income <= 26750) {
                state_stfc_recd <- state_stfc_recd_levels[match(max(income_levels[income_levels <= state_gross_income]), income_levels)]
            } else {
                state_stfc_recd <- 0
            }
        } else if (state_filing_status == "Married") {
            if (child_number == 0) {
                income_levels <- c(41500, 42500, 43500, 44500, 45500, 46500, 47500, 48500, 49500, 50500, 51500, 52500, 53500)
                state_stfc_recd_levels <- c(175, 155, 135, 115, 95, 75, 55, 35, 15, 0, 0, 0, 0)
                if (state_gross_income <= 42500) {
                    state_stfc_recd <- 175
                } else if (state_gross_income > 42500 & state_gross_income <= 53500) {
                    state_stfc_recd <- state_stfc_recd_levels[match(max(income_levels[income_levels <= state_gross_income]), income_levels)]
                } else {
                    state_stfc_recd <- 0
                }
            } else if (child_number == 1) {
                income_levels <- c(41500, 42500, 43500, 44500, 45500, 46500, 47500, 48500, 49500, 50500, 51500, 52500, 53500)
                state_stfc_recd_levels <- c(200, 180, 160, 140, 120, 100, 80, 60, 40, 20, 0, 0, 0)
                if (state_gross_income <= 42500) {
                    state_stfc_recd <- 200
                } else if (state_gross_income > 42500 & state_gross_income <= 53500) {
                    state_stfc_recd <- state_stfc_recd_levels[match(max(income_levels[income_levels <= state_gross_income]), income_levels)]
                } else {
                    state_stfc_recd <- 0
                }
            } else { # if ($in->{'child_number'} > 1) {
                income_levels <- c(41500, 42500, 43500, 44500, 45500, 46500, 47500, 48500, 49500, 50500, 51500, 52500, 53500)
                state_stfc_recd_levels <- c(225, 205, 185, 165, 145, 125, 105, 85, 65, 45, 25, 5, 0)
                if (state_gross_income <= 42500) {
                    state_stfc_recd <- 225
                } else if (state_gross_income > 42500 & state_gross_income <= 53500) {
                    state_stfc_recd <- state_stfc_recd_levels[match(max(income_levels[income_levels <= state_gross_income]), income_levels)]
                } else {
                    state_stfc_recd <- 0
                }
            }
        } else { # $state_filing_status eq 'Head of Household'
            if (child_number == 0) {
                income_levels <- c(31000, 31850, 32600, 33350, 34100, 34850, 35600, 36350, 37100, 37850, 38600, 39350, 40100, 40850, 41600, 42350)
                state_stfc_recd_levels <- c(175, 160, 145, 130, 115, 100, 85, 70, 55, 40, 25, 10, 0, 0, 0, 0)
                if (state_gross_income <= 31850) {
                    state_stfc_recd <- 175
                } else if (state_gross_income > 31850 & state_gross_income <= 42350) {
                    state_stfc_recd <- state_stfc_recd_levels[match(max(income_levels[income_levels <= state_gross_income]), income_levels)]
                } else {
                    state_stfc_recd <- 0
                }
            } else if (child_number == 1) {
                income_levels <- c(31000, 31850, 32600, 33350, 34100, 34850, 35600, 36350, 37100, 37850, 38600, 39350, 40100, 40850, 41600, 42350)
                state_stfc_recd_levels <- c(200, 185, 170, 155, 140, 125, 110, 95, 80, 65, 50, 35, 20, 5, 0, 0)
                if (state_gross_income <= 31850) {
                    state_stfc_recd <- 200
                } else if (state_gross_income > 31850 & state_gross_income <= 42350) {
                    state_stfc_recd <- state_stfc_recd_levels[match(max(income_levels[income_levels <= state_gross_income]), income_levels)]
                } else {
                    state_stfc_recd <- 0
                }
            } else { # more than 1 kid
                income_levels <- c(31000, 31850, 32600, 33350, 34100, 34850, 35600, 36350, 37100, 37850, 38600, 39350, 40100, 40850, 41600, 42350)
                state_stfc_recd_levels <- c(225, 210, 195, 180, 165, 150, 135, 120, 105, 90, 75, 60, 45, 30, 15, 0)
                if (state_gross_income <= 31850) {
                    state_stfc_recd <- 225
                } else if (state_gross_income > 31850 & state_gross_income <= 42350) {
                    state_stfc_recd <- state_stfc_recd_levels[match(max(income_levels[income_levels <= state_gross_income]), income_levels)]
                } else {
                    state_stfc_recd <- 0
                }
            }
        }
    }
    total_refundable_credits <- state_cadc_refundable + state_adultcare_credit_refundable + state_eitc_recd + state_ptfc_recd + state_stfc_recd
    
    #We can now calculate the final outputs:
    state_tax <- state_tax_net - total_refundable_credits
    state_tax_credits <- total_nonrefundable_credits + total_refundable_credits
    
    # Determine local taxes, if any:
    # As indicated above, there are no local income taxes in Maine.
    
    # We use the term "tax before credits" to refer to tax before refundable credits, as it is used for the "total expenses" calculation, but this may deserve some rethinking.
    tax_before_credits <- federal_tax_gross + state_tax_gross
    tax_after_credits <- tax_before_credits - federal_tax_credits - state_tax_credits
    
    outputs_hash[[ 'state_tax' ]] <- state_tax
    outputs_hash[[ 'county_tax' ]] <- county_tax
    outputs_hash[[ 'state_tax_gross' ]] <- state_tax_gross
    outputs_hash[[ 'state_tax_credits' ]] <- state_tax_credits
    outputs_hash[[ 'tax_before_credits' ]] <- tax_before_credits
    outputs_hash[[ 'tax_after_credits' ]] <- tax_after_credits
    outputs_hash[[ 'dep_exempt_credit_potential' ]] <- dep_exempt_credit_potential
    outputs_hash[[ 'state_cadc_nonrefundable' ]] <- state_cadc_nonrefundable
    outputs_hash[[ 'state_adultcare_credit_nonrefundable' ]] <- state_adultcare_credit_nonrefundable
    outputs_hash[[ 'state_cadc_refundable' ]] <- state_cadc_refundable
    outputs_hash[[ 'state_adultcare_credit_refundable' ]] <- state_adultcare_credit_refundable
    outputs_hash[[ 'state_eitc_recd' ]] <- state_eitc_recd
    outputs_hash[[ 'state_ptfc_recd' ]] <- state_ptfc_recd
    outputs_hash[[ 'state_stfc_recd' ]] <- state_stfc_recd
    
}

# Food, 2020.
food_module <- function(family_structure, child_age, parent1_age, parent2_age, family_size, food_override, food_override_amt, wic_recd, child_foodcost_red_total) {
    
    food_expenses <- 0 # total annual family food expenses
    family_foodcost <- 0 # annual
    subsidized_food <- 0 # the total amount of food subsidies that the family is receiving
    
    # Variables used in this module.
    # Costs for low-cost food plan cited below, including food cost adjustments, are available at  https://fns-prod.azureedge.net/sites/default/files/media/file/CostofFoodJan2020.
    parent1_foodcost_m <- 227.20 # monthly food cost for first parent (Mean of low-cost food costs between Female aged 19-50 years and Male aged 19-50 years. Source is the USDA Low-Cost food plan issued Jan 2020.)
    parent2_foodcost_m <- 227.20 # monthly food cost for second parent. (Mean of low-cost food costs between Female aged 19-50 years and Male aged 19-50 years. Source is the USDA Low-Cost food plan issued Jan 2020.)
    olderparent_foodcost_m <- 218.15 #  (Mean of low-cost food costs between Female aged 51-70 years and Male aged 51-70 years. Source is the USDA Low-Cost food plan issued Jan 2020.)
    yo18parent_foodcost_m <- 226.80 # (Mean of low-cost food costs between 18-year-old female and 18-year-old male. Source is the USDA Low-Cost food plan issued Jan 2020. That this is only 80 cents lower than the monthly parent1 and parent2 food cost indicated above makes this a pretty marginal adjustment for this year, but as we are pulling from a federal table that specifically includes ages, it is appropriate to do so at this time. If the table indicates larger differences between adults and 18-year-olds at a later date, this code will already account for that change.
    child1_foodcost_m <- 0 # Monthly food cost per child stratified by age
    child2_foodcost_m <- 0 # Monthly food cost per child stratified by age
    child3_foodcost_m <- 0 # Monthly food cost per child stratified by age
    child4_foodcost_m <- 0 # Monthly food cost per child stratified by age
    child5_foodcost_m <- 0 # Monthly food cost per child stratified by age
    family_size_adjustment <- c(0, 1.2, 1.1, 1.05, 1, 0.95, 0.95, 0.90)[family_size]
    
    # Calculated in macro.
    base_foodcost_m <- 0 # Total monthly (unadjusted) family food cost, based on a family of 4
    base_foodcost <- 0 # NIP: Total annual (unadjusted) family food cost, based on a family of 4. Commenting out for now; delete this if there's no other program that uses this. but comment out if it's not mentioned anywhere else.
    family_foodcost_fmred <- 0 # NIP: the family food costs after accounting for free meals programs for children.
    
    # 1. Calculate base food cost for each family.
    # Use Food Cost tables to look up child_foodcost.
    
    if (parent1_age > 50) {
        parent1_foodcost_m <- olderparent_foodcost_m
    } else if(parent1_age == 18) {
        parent1_foodcost_m <- yo18parent_foodcost_m
    }
    
    if (family_structure == 1) {
        parent2_foodcost_m <- 0
    } else if (parent2_age > 50) {
        parent2_foodcost_m <- olderparent_foodcost_m
    } else if (parent2_age == 18) {
        parent2_foodcost_m <- yo18parent_foodcost_m
    }
    
    # Initializing child food cost table (from KY FRS testing spreadsheet).
    child_food_costs <- data.frame(age_min = c(0, 2, 4, 6, 9, 12, 14), age_max = c(1, 3, 5, 8, 11, 13, 18), 
                                   cost = c(129.30, 135.20, 138.70, 194.50, 208.80, 224.60, 226.80))
    
    num_children <- length(child_age)
    for (i in 1:num_children) {
        age <- child_age[[ toString(i) ]]
        cost <- child_food_costs$`cost`[age >= child_food_costs$`age_min` & age <= child_food_costs$`age_max`]
        assign(paste0("child", i, "_foodcost_m"), cost)
    }
    
    base_foodcost_m <- parent1_foodcost_m + parent2_foodcost_m + child1_foodcost_m + child2_foodcost_m + 
        child3_foodcost_m + child4_foodcost_m + child5_foodcost_m
    
    family_foodcost <- base_foodcost_m * 12 * family_size_adjustment
    
    if (food_override == "other") {
        food_expenses <- 12 * nullCheck(as.numeric(food_override_amt))
    } else {
        food_expenses <- pos_sub(family_foodcost - nullCheck(child_foodcost_red_total), wic_recd)
    }
    
    subsidized_food <- food_expenses - family_foodcost
    
    outputs_hash[[ "food_expenses" ]] <<- food_expenses
    outputs_hash[[ "family_foodcost" ]] <<- family_foodcost
    outputs_hash[[ "subsidized_food" ]] <<- subsidized_food
    
}

# Lifeline, 2020.
lifeline_module <- function(lifeline, earnings, fpl, hlth_cov_parent, hlth_cov_children, fsp_recd, housing_recd, ssi_recd, medically_needy) {
    
    # Create variables.
    
    lifeline_subsidy <- 9.25 #   The monthly subsidy for those participating in the Lifeline program
    lifeline_inc_limit <- 1.35 #   The income eligibility limit as a % of federal poverty guideline
    
    # Outputs used in this module.
    
    lifeline_recd <- 0
    
    # 1: Check for Lifeline flag
    
    if (lifeline == 0) {
        lifeline_recd <- 0
    } else {
        
        # 2: Check for Determine subsidy if eligible
        #
        # Eligibility criteria based on requirements listed in Federal Register / Vol. 77, No. 42 / Friday, March 2, 2012, § 54.409, with adjustments based on changes as part of the Lifeline modernization plan enacted in 2016 (see https://www.fcc.gov/general/lifeline-program-low-income-consumers). Note a change for 2019 is that we have reinserted categorical eligibility for medically needy programs, such as Florida had in 2015; this is technically Medicaid.
        
        if (earnings / fpl <= lifeline_inc_limit | hlth_cov_parent == "Medicaid" | hlth_cov_parent == "Medicaid and private" | 
            sum(values(hlth_cov_children) == "Medicaid") > 0 | fsp_recd > 0 | housing_recd > 0 | ssi_recd > 0 | medically_needy == 1) {
            lifeline_recd <- lifeline_subsidy * 12
        } else {
            lifeline_recd <- 0
        }
        
    }
    
    outputs_hash[[ "lifeline" ]] <<- lifeline
    outputs_hash[[ "lifeline_subsidy" ]] <<- lifeline_subsidy
    outputs_hash[[ "lifeline_inc_limit" ]] <<- lifeline_inc_limit
    outputs_hash[[ "lifeline_recd" ]] <<- lifeline_recd
    
}

# Other Expenses, 2020.
other_expenses <- function(rent_paid, disability_work_expenses_m, disability_personal_expenses_m, family_foodcost, lifeline_recd, child_number, Other_necessities_cost) {
    
    rent_cost_m <- rent_paid / 12
    rent_cost <- rent_paid
    family_foodcost <- outputs_hash[[ "family_foodcost" ]]
    lifeline_recd <- outputs_hash[[ "lifeline_recd" ]]
    
    # Outputs created.
    other_expenses <- 0
    other_expenses_postsalestax <- 0
    disability_expenses <- 0
    
    # We formerly relied on EPI’s family budget calculator’s method of determining the cost of other necessities, which is based on Consumer Expenditure Survey data: http://www.epi.org/publication/family-budget-calculator-technical-documentation/. This estimates that the cost of items including “apparel, entertainment, personal care expenses, household supplies (including furnishings and equipment, household operations, housekeeping supplies, and telephone services), reading materials, school supplies, and other miscellaneous items of necessity” total to 48.3% of the cost of food and housing, based on 2014 Consumer Expenditure Survey data. This is a change from an earlier methodology that EPI used, that pegged this at 25.6% of food and housing. Upon closer analysis of their approach in August 2017, we decided to slightly adjust it by removing educational expenses (which includes private tuition and other costs that we already account for, elsewhere,  such as afterschool co-pays) as well as entertainment costs, which seems more a function of disposable income than costs that should be included in calculations for a basic, livable income. Based on 2015 Consumer Expenditure Survey data, the removal of these expenses brought the proportion of these costs compared to rent, utilities, and food down to 34%. Based on the 2017-2018 Consumer Expenditure Survey data, these same categories constituted 33.6% of spending.
    # Note that rent_cost is included in here, not rent_paid. This represents the unsubsidized cost of housing, so that receipt of Section 8 does not impact costs of other necessities.
    other_expenses_percentage <- 0.336
    sales_tax_average <- 0.0739 # To enable state-by-state comparisons, and to better estimate the impact of sales tax policy, beginning in 2019 we are reducing the calculation of other expenses by the average combined state and local tax rate, which is calculated annually by the Tax Foundation. That annual report does not include a national average, but that can easily be determined by weighting each of the state average rates by the population in that state relative to the population of the US. This is calculated from using the latest Tax Foundation publication on average sales tax rates by state, and finding a national average using the latest Census state popuilations, to weigh the state averages against the proportion they represent of the national population. For the 2019 Tax Foundation publication, this is the average sales tax facing Americans. According to that publication (https://files.taxfoundation.org/20190130115700/State-Local-Sales-Tax-Rates-2019-FF-633.pdf), most sales tax calculations use a base that is generally consistent with our calculation for other expenses. 2020 documentation is available at https://files.taxfoundation.org/20200115132659/State-and-Local-Sales-Tax-Rates-2020.pdf. 
    
    if (!is.null(Other_necessities_cost)) {
        other_expenses_postsalestax <- pos_sub(input$Other_necessities_cost * 12, lifeline_recd)
    } else {
        # New for 2019, we're going to calculate this as a pre-sales tax base, meaning we're going to adjust this figure to remove the national average of sales taxes.
        other_expenses_postsalestax <- pos_sub(other_expenses_percentage * family_foodcost + rent_paid, lifeline_recd)
    }
    
    # Now we adjust to remove the estimated sales tax portion. This allows us to have an "other expenses" base that is unchanged by state or local sales taxes, enabling comparisons of sales tax policies across states and localities.	
    
    other_expenses <- other_expenses_postsalestax / (1 + sales_tax_average)
    
    # Beginning in 2017, we will also be including disability-related expenses, derived from new user-entered inputs.
    
    disability_expenses <- 12 * disability_work_expenses_m + disability_personal_expenses_m
    
    outputs_hash[[ "other_expenses" ]] <<- other_expenses
    outputs_hash[[ "disability_expenses" ]] <<- disability_expenses
    outputs_hash[[ "other_expenses_postsalestax" ]] <<- other_expenses_postsalestax
    
}

# Sales Tax, Maine 2020.
salestax_module <- function(other_expenses) {
    
    state_sales_tax_rate_other <- 0.055
    
    # The ME sales tax on tangible personal property. See  https://www.maine.gov/revenue/salesuse/salestax/ReferenceGuide2019.pdf. 
    
    local_sales_tax_rate_other <- 0
    # There are  no local sales taxes in ME . https://legislature.maine.gov/legis/bills/display_ps.asp?LD=1254&snum=129. See also https://smartasset.com/taxes/maine-tax-calculator.
    
    #In most places, the only applicable sales tax rate for the purposes of the FRS/MTRC is a tax rate on tangible personal property, and these expenditures are captured completely in the “other” expenses calculation. It is important to consider what expenses are additionally included in state or local sales tax systems beyond the expenses captured in sales taxes. IN THE CASE OF MAINE, WE MAY NEED TO INCORPORATE CERTAIN ELECTRICITY COSTS. 
    
    # Combining state and local taxes:
    sales_tax_rate_other <- state_sales_tax_rate_other + local_sales_tax_rate_other
    
    # Because our calculation of “other expenses” is based on the EPI calculation of other expenses, and because that in turn is based on national consumer expenditure statistics (which cannot be easily broken down by state), the below calculation carves out sales taxes from the “other expenses” calculation. This calculation is different than the one we did in 2017, when we introduced sales tax, because the "other expenses" calculation is now reduced by the national sales tax average. This allows for better state-to-state comparisons.
    
    # Note: ME also charges on residential electric use after the first 750 KwH/month. This is something we may need to address for homes that rely on electric for heating, and may also be an issue for homes that rely on non-electric sources (e.g. natural gas) for cooking. See https://www.eia.gov/tools/faqs/faq.php?id=97&t=3 for survey data.
    
    sales_tax <- sales_tax_rate_other * other_expenses 
    
    outputs_hash[[ "sales_tax" ]] <- sales_tax
    outputs_hash[[ "sales_tax_rate_other"]] <- sales_tax_rate_other
    
}




ui <- fluidPage(
                
                tags$h2("Family Resource Simulator (Maine 2020) in R Shiny"), 
                br(),
                shinyjs::useShinyjs(),
                 
                sidebarLayout(
                        sidebarPanel(
                            tabsetPanel(
                                tabPanel("1. Geography",
                                     tags$h3("Select county and town."),
                                     selectInput("selected_county","County",counties, "Oxford County"),
                                     uiOutput("narrowed_down_town_list"),
                                ),
                                tabPanel("2. Family",
                                     tags$h3("Select family characteristics."),
                                     selectInput("family_structure", "How many parents or adults are in the household? ",
                                                 c(1, 2), 2),
                                     uiOutput("family_structure_ages"),
                                     numericInput("children_count", "How many children are in the household? ",
                                                 2, 1, 5, 1),
                                     uiOutput("children_count_ages"),
                                     selectInput("breastfeeding", "Does the mother breastfeeding any children?", 
                                                 c("Yes" = 1, "No" = 0)),
                                     selectInput("disability_parent1", "Does the primary wage earner in the house have 
                                                 a disability?", c("Yes" = 1, "No" = 0), 0),
                                     selectInput("disability_parent2", "Does the secondary wage earner in the house have 
                                                 a disability?", c("Yes" = 1, "No" = 0), 0),
                                     selectInput("cs_flag", "Does the family receive child support from a 
                                                 non-custodial parent?", c("Yes" = "Yes", "No" = "No"))
                                ),
                                tabPanel("3. Income & Assets",
                                     tags$h3("Enter income sources, assets, and debt."),
                                     textInput("wage_1", "Parent 1 wage rate: ", 7.25),
                                     uiOutput("second_parent_wage"),
                                     selectInput("nontraditionalwork", "Do parent(s) in the family work 
                                                 nontraditional hours?",
                                                 c("Yes" = "Yes", "No" = "No"), "Yes"),
                                     uiOutput("nontraditional_details"),
                                     tags$h4("Assets and debt: "),
                                     textInput("savings", "Amount of family savings (in checking or savings
                                               account)", 0),
                                     selectInput("vehicles", "How many cars does the family have?",
                                                 c(0, 1, 2), 1),
                                     uiOutput("vehicles"),
                                     textInput("debt_payment", "Monthly Debt payment (e.g., credit cards,
                                               medical debt, car payments)", 0),
                                     uiOutput("child_support")
                                ),
                                tabPanel("4. Work Supports",
                                     tags$h3("Select work supports."),
                                     tags$h4("The simulator is capable of modeling the impact of the below programs,
                                             benefits, and work supports."),
                                     checkboxGroupInput("work_supports", "Select which benefits the family receives
                                             if/when eligible: ", 
                                                        c(#"Child Care and Development Fund Subsidies (CCAP)" = "ccdf",
                                                          "SNAP/Food Stamps" = "fsp",
                                                          "Public Health Insurance (Medicaid, SCHIP)" = "hlth",
                                                          "Section 8 Housing Vouchers" = "sec8",
                                                          #"TANF Cash Assistance (KTAP)" = "tanf",
                                                          "Lifeline" = "lifeline",
                                                          #"Low Income Energy Assistance Program (LIHEAP)" = "liheap",
                                                          "Women, Infants, and Children (WIC)" = "wic",
                                                          "Supplemental Security Income (SSI)" = "ssi",
                                                          #"National School Breakfast Program (NSBP)" = "nsbp",
                                                          #"National School Lunch Program (NSLP)" = "frpl",
                                                          #"Free Summer Meals Program (FSMP)" = "fsmp",
                                                          "Earned Income Tax Credit (EITC)" = "eitc",
                                                          "Child Tax Credit" = "ctc"), c("ssi", "wic", "ctc", "eitc", "sec8"))
                                                          #"Child and Dependent Care Tax Credit" = "cadc",
                                                          #"Premium Tax Credit" = "premium_tax_credit"))
                                ),
                                tabPanel("5. Child Care",
                                     tags$h3("Make choices about child care."),
                                     tags$h4("Select setting for subsidized child care."),
                                     uiOutput("children_care_setting"),
                                     tags$h4("Select setting or enter cost for unsubsidized child care."),
                                     radioButtons("select_or_cost", "", 
                                                  c("Select setting" = "select_setting",
                                                    "Enter cost" = "enter_cost")),
                                     uiOutput("unsub_child_care_cost")
                                ),
                                tabPanel("6. Health Insurance", # Hasn't really been integrated yet!
                                     tags$h3("Choose private health insurance option when family loses public coverage."),
                                     tags$h4("Private health insurance cost estimates include insurance premiums only,
                                             not copayments or other out-of-pocket expenses. The below estimates will
                                             represent the cost of health care when the family is not eligible for
                                             Medicaid or cost-reducing premium tax credits."),
                                    radioButtons("privateplan_type", "Choose health insurance option: ", 
                                                c("Employer-based plan: $136 per month for parent(s) (when children
                                                  are still eligible for public insurance)\n$449 per month for family\n"
                                                  = "employer_based",
                                                  "Individual/Nongroup plan\n$400 per month for parents(s)\n
                                                  $964 per month for family\n" = "individual",
                                                  "Other cost estimate" = "other")),
                                    uiOutput("other_cost_estimate"),
                                    textInput("additional_out_of_pocket_exp", "Additional out-of-pocket medical
                                              expenses per month: ", 25),
                                    uiOutput("disability_medical")
                                ),
                                tabPanel("7. Other Expenses",
                                    tags$h3("Select Family Resource Simulator estimate or enter your own estimate."),
                                    tags$h4("Rent (cost when unsubsidized)"),
                                    selectInput("home_type", "Home Type", c("Apartment", "House")),
                                    radioButtons("home_cost", "Home Cost", 
                                                 c("Fair Market Rent (including utilities), as determined by the 
                                                   U.S. Department of Housing and Urban Development: $631 per
                                                   month" = "hud_rent",
                                                   "Other rental cost estimate (including utilities)" = "other")),
                                    uiOutput("other_rental_cost"),
                                    tags$h4("Utilities (portion of rent)"),
                                    selectInput("energy_source", "Energy Source", 
                                                c("Natural Gas" = "nat_gas",
                                                  "Electric" = "electric",
                                                  "Bottle Gas" = "bottle",
                                                  "Coal" = "coal",
                                                  "Wood"= "wood",
                                                  "Oil" = "oil")),
                                    checkboxInput("heat_in_rent", "Are utilities included in the rent?"),
                                    radioButtons("utilities_cost", "Utilities Cost", 
                                                 c("Utility cost based on local Public Housing Authority estimates"
                                                   = "pha_cost", 
                                                   "Other energy cost estimate (can only select if choosing 'Other 
                                                   rental cost estimate' above)" = "other")),
                                    uiOutput("other_utilities_cost"),
                                    tags$h4("Food"),
                                    radioButtons("food_cost", "Select cost per month", 
                                                 c("Low-Cost Food Plan developed by the U.S. Department of 
                                                   Agriculture" = "usda", "Other cost estimate" = "other" 
                                                   )),
                                    uiOutput("other_food_cost"),
                                    tags$h4("Transportation"),
                                    radioButtons("transport_cost", "Select cost per month", 
                                                 c("Private transportation cost estimate" = "private",
                                                   "Other cost estimate" = "other")),
                                    uiOutput("other_transport_cost"),
                                    uiOutput("disability_check"),
                                    tags$h4("Other necessities"),
                                    radioButtons("necessities_cost", "Select cost per month",
                                                 c("Family Resource Simulator estimate of other necessities" 
                                                   = "FRS", "Other cost estimate" = "other")),
                                    uiOutput("other_necessities_cost"),
                                )
                            )
                        ),
                        mainPanel(
                            plotOutput("resource_plot")
                        )
                 )
                 
)

server <- function(input, output, session) {
    
    # An assortment of helper reactive functions (that respond dynamically to changing inputs).
    
    child_age_hash <- reactive({
        child_age <- hash()
        num_children <- as.numeric(input$children_count)
        for (i in 1:num_children) {
            child_age[[ toString(i) ]] <- eval(parse(text=paste0("input$child", i, "_age")))
        }
        return(child_age)
    })
    
    child_health_hash <- reactive({
        child_health <- hash()
        num_children <- as.numeric(input$children_count)
        for (i in 1:num_children) {
            # Assume all children are all on Medicaid.
            child_health[[ toString(i) ]] <- "employer"
        }
        return(child_health)
    })
    
    child_lunch_hash <- reactive({
        child_lunch <- hash()
        num_children <- as.numeric(input$children_count)
        for (i in 1:num_children) {
            child_lunch[[ toString(i) ]] <- 0
        }
        return(child_lunch)
    })
    
    work_supports <- reactive({
        selected_supports <- input$work_supports
        supports <- hash(keys = c("fsp", "hlth", "sec8", "lifeline", "wic", "ssi", "eitc", "ctc"), values = 0)
        for (key in keys(supports)) {
            if (is.element(key, selected_supports)) {
                supports[[ key ]] <- 1
            }
        }
        return(supports)
    })
    
    family_size <- reactive({
        num_children <- as.integer(input$children_count)
        num_parents <- as.integer(input$family_structure)
        return(num_children + num_parents)
    })
    
    vehicles_hash <- reactive({
        num_vehicles <- as.integer(input$vehicles)
        vehicles <- hash()
        vehicles[[ "vehicle1_value" ]] <- 0
        vehicles[[ "vehicle1_owed" ]] <- 0
        vehicles[[ "vehicle2_value" ]] <- 0
        vehicles[[ "vehicle2_owed" ]] <- 0
        if (num_vehicles == 0) {
            return()
        } else if (num_vehicles == 1) {
            vehicles[[ "vehicle1_value" ]] <- input$vehicle1_value
            vehicles[[ "vehicle1_owed" ]] <- input$vehicle1_owed
        } else {
            vehicles[[ "vehicle1_value" ]] <- input$vehicle1_value
            vehicles[[ "vehicle1_owed" ]] <- input$vehicle1_owed
            vehicles[[ "vehicle2_value" ]] <- input$vehicle2_value
            vehicles[[ "vehicle2_owed" ]] <- input$vehicle2_owed
        }
        return(vehicles)
    })
    
    # Other UI outputs.
    
    output$narrowed_down_town_list <- renderUI({
        towns_displayed <- maine_localities$"name"[maine_localities$"countyname" == input$selected_county]
        selectInput("selected_town", "Town", towns_displayed, "Appleton")
    })
    
    output$family_structure_ages <- renderUI({}) # To make sure UI is rendered at startup.
    outputOptions(output, "family_structure_ages", suspendWhenHidden = FALSE)
    output$family_structure_ages <- renderUI({
        num_parents <- as.integer(input$family_structure)
        lapply(1:num_parents, function(i) {
            numericInput(paste0("parent", i,"_age"), paste("Age of parent ", i), 28, 18, 61, 1)
        })
    })
    
    output$children_count_ages <- renderUI({}) # To make sure UI is rendered at startup.
    outputOptions(output, "children_count_ages", suspendWhenHidden = FALSE)
    output$children_count_ages <- renderUI({
        num_children <- as.integer(input$children_count)
        lapply(1:num_children, function(i) {
            numericInput(paste0("child", i, "_age"), paste("Age of child ", i), 6, 0, 17, 1)
        })
    })
    
    output$disability_check <- renderUI({})
    outputOptions(output, "disability_check", suspendWhenHidden = FALSE)
    output$disability_check <- renderUI({
        if (input$disability_parent1 == 1 | input$disability_parent2 == 1) {
            tagList(
                tags$h4("Disability-related expenses"),
                textInput("disability_personal_expenses_m", "Additional personal (non-work-related) expenses needed by disabled adult(s)
                          in the household:", 0),
                textInput("disability_work_expenses_m", "Additional expenses needed for items or services that disabled parent(s) needs
                          in order to work:", 0)
            )
        }
    })
    
    output$second_parent_wage <- renderUI({}) # To make sure UI is rendered at startup.
    outputOptions(output, "second_parent_wage", suspendWhenHidden = FALSE)
    output$second_parent_wage <- renderUI({
        if (input$family_structure == 2) {
            tagList(
                textInput("wage_parent2", "Parent 2 wage rate: ", 7.25),
                selectInput("parent2_max_work", "Employment of second parent or adult", 
                            c("Full-time" = "full_time",
                              "Part-time" = "part_time",
                              "Not employed" = "not_employed"))
            )
        }
    })
    
    output$nontraditional_details <- renderUI({
        if (input$nontraditionalwork == "Yes") {
            tagList(
                sliderInput("parent1_max_work", "Up to how many hours does the first parent work in a week?", 0, 40, 40),
                sliderInput("maxshiftlength_parent1", "How many hours are in each shift for the first parent?", 
                            0, 24, 8),
                selectInput("maxworkweek_parent1", "Up to how many days per week does the first parent work?", 1:7, 5),
                selectInput("backtobackshifts_parent1", "Number of hours of travel time between shifts for the first
                            parent (up to two hours)", 0:2, 1),
                selectInput("weekenddaysworked", "Maximum number of weekend days worked", 0:2, 1),
                sliderInput("maxweekendshifts", "Up to how many shifts does the parent work per day during the
                             weekend?", 0, 24, 0),
                textInput("parent2_max_work_override_amt", "If second parent works at most a different number of hours per
                          week than 0 (Not employed), 20 (Part-time), or 40 (Full-time), enter that value", 40),
                sliderInput("maxshiftlength_parent2", "How many hours are in each shift for the second parent?", 1, 24, 0),
                selectInput("maxworkweek_parent2", "Up to how many days per week does the second parent work?", 1:7),
                textInput("parent1_first_max", "How many hours per week will the first parent work in a week before
                          the second parent begins working?", 40),
                selectInput("backtobackshifts_parent2", "Number of hours of travel time between shifts for the second
                            parent (up to two hours)", 0:2, 1),
                textInput("breadwinner_wkday_hometime", "When both parents are working, up to how many hours per week
                          is one parent at home on the WEEKDAYS while the other parent is working or traveling to work?",
                          0),
                textInput("breadwinner_wkend_hometime", "When both parents are working, up to how many hours per week
                          is one parent at home on the WEEKENDS while the other parent is working or traveling to work?", 0),
                selectInput("workdaystart", "When does the (least-working) parent start work?",
                            c("1:00 am", "2:00 am", "3:00 am", "4:00 am", "5:00 am", "6:00 am", "7:00 am", "8:00 am",
                              "9:00 am", "10:00 am", "11:00 am", "12:00 pm", "1:00 pm", "2:00 pm", "3:00 pm", "4:00 pm",
                              "5:00 pm", "6:00 pm", "7:00 pm", "8:00 pm", "9:00 pm", "10:00 pm", "11:00 pm", "12:00 am"))
            )
        }
    })
    
    output$vehicles <- renderUI({
        if (input$vehicles == 0) {
            return()
        } else if (input$vehicles == 1) {
            tagList(
                textInput("vehicle1_value", "Value of the family's car: ", 0),
                textInput("vehicle1_owed", "Amount family owes on car: ", 0),
            )
        } else {
            tagList(
                textInput("vehicle1_value", "Value of the family's first car: ", 0),
                textInput("vehicle1_owed", "Amount family owes on first car: ", 0),
                textInput("vehicle2_value", "Value of the family's second car: ", 0),
                textInput("vehicle2_owed", "Amount family owes on second car: ", 0)
            )
        }
    })
    
    output$child_support <- renderUI({
        
    })
    
    output$children_care_setting <- renderUI({
        num_children <- as.integer(input$children_count)
        lapply(1:num_children, function(i) {
            selectInput(paste0("child_", i), paste("Child care setting for child ", i), 
                        c("Licensed Type 1 Provider ($28 per day)" = 28,
                          "Licensed Type 2 Provider ($30 per day)" = 30,
                          "Certified Family Child Care Home ($23 per day)" = 23,
                          "Registered Provider (family, friend, or neighbor) ($0 per day)" = 0))
        })
    })
    
    output$other_cost_estimate <- renderUI({
        if (input$private_plan == "other") {
            tagList(
                textInput("parents_health_cost", "Cost per month for parent(s): "),
                textInput("family_health_cost", "Cost per month for family: "),
                checkboxInput("cost_estimate_plan_type", "Select if other cost estimate is from
                                              the individual/nongroup market. (If not checked, the simulator 
                                              assumes this is an employer-provided plan.)")
            )
        }
    })
    
    output$disability_medical <- renderUI({
        if (input$disability_parent1 == 1 | input$disability_parent2 == 1) {
            tagList(
                tags$h4("Disability-related expenses"),
                textInput("disability_medical_expenses_mnth", "The amount of these medical expenses related to parent disability", 0)
            )
        }
    })
    
    output$unsub_child_care_cost <- renderUI({
        num_children <- as.integer(input$children_count)
        if (input$select_or_cost == "select_setting") {
            lapply(1:num_children, function(i) {
                selectInput(paste0("child_", i), paste("Child care setting for child ", i), 
                            c("Licensed Type 1 Provider ($28 per day)" = 28,
                              "Licensed Type 2 Provider ($30 per day)" = 30,
                              "Certified Family Child Care Home ($23 per day)" = 23,
                              "Registered Provider (family, friend, or neighbor) ($0 per day)" = 0))
            })
        } else {
            lapply(1:num_children, function(i) {
                textInput(paste0("child_", i), paste("Cost per day for child ", i), 0)
            })
        }
        
    })
    
    output$other_rental_cost <- renderUI({
        if (input$home_cost == "other") {
            textInput("other_rental_cost_estimate", "Rent cost per month: ", 0)
        }
    })
    
    output$other_utilities_cost <- renderUI({
        if (input$utilities_cost == "other") {
            textInput("other_utilities_cost", "Utilities cost per month", 0)
        }
    })
    
    observeEvent(input$home_cost, {
        if (input$home_cost == "other") {
            updateRadioButtons(session, "utilities_cost", "Utilities Cost", 
                               c("Utility cost based on local Public Housing Authority estimates"
                                 = "pha_cost", 
                                 "Other energy cost estimate (can only select if choosing 'Other 
                             rental cost estimate' above)" = "other"), "other")
        } else {
            updateRadioButtons(session, "utilities_cost", "Utilities Cost", 
                               c("Utility cost based on local Public Housing Authority estimates"
                                 = "pha_cost", 
                                 "Other energy cost estimate (can only select if choosing 'Other 
                             rental cost estimate' above)" = "other"), "pha_cost")
        }
    })
    
    output$other_food_cost <- renderUI({
        if (input$food_cost == "other") {
            textInput("other_food_cost", "Other monthly food cost estimate", 0)
        }
    })
    
    output$other_transport_cost <- renderUI({
        if (input$transport_cost == "other") {
            textInput("other_transport_cost", "Other monthly transport cost estimate", 0)
        }
    })
    
    output$other_necessities_cost <- renderUI({
        if (input$necessities_cost == "other") {
            textInput("Other_necessities_cost", "Other monthly necessities cost estimate", 0)
        }    
    })
    
    FRS <- reactive({
        
        #validate(need(input$family_structure, "family structure"))
        #validate(need(input$parent1_age, "check parent 1 age"))
        
        datalist = list() # To store net_resource outputs at various earnings levels.
        
        outputs_hash[[ "debt_payment" ]] <- as.integer(input$debt_payment) * 12
        
        interval = 1000
        for (i in seq(from=0, to=70000, by=interval)) {
            
            earnings <- i
            earnings_mnth <- earnings / 12
            earnings_wk <- earnings / 52
            
            # Definitely want to spend some time cleaning up how we call these functions and plug in inputs.
            
            interest_module(input$savings, 0.0006)
            
            parent_earnings_module(earnings, input$family_structure, input$nontraditionalwork, input$parent1_first_max, input$parent1_max_work, input$parent2_max_work_override_amt, input$parent2_max_work, input$maxshiftlength_parent1, input$maxshiftlength_parent2, input$maxworkweek_parent1, input$maxworkweek_parent2, input$backtobackshifts_parent1, input$backtobackshifts_parent2, input$wage_1, input$wage_parent2)
            
            ssp_module()
            
            ssi_module(work_supports()[[ "ssi" ]], input$savings, vehicles_hash(), input$family_structure, input$disability_parent1, input$disability_parent2, input$disability_work_expenses_m, length(child_age_hash()), outputs_hash[[ "interest_m" ]], earnings_mnth, outputs_hash[[ "parent1_earnings" ]], outputs_hash[[ "parent2_earnings" ]], outputs_hash[[ "ssp_couple_ben" ]], outputs_hash[[ "ssp_individual_ben" ]], outputs_hash[[ "ssp_couple_thresh" ]], outputs_hash[[ "ssp_individual_thresh" ]])
            
            fed_hlth_insurance_module(earnings_mnth, outputs_hash[[ "interest_m" ]], family_size(), input$disability_parent1, input$disability_parent2, outputs_hash[[ "ssi_recd" ]], outputs_hash[[ "parent1_earnings" ]], outputs_hash[[ "parent2_earnings" ]])
            
            calculated_rent = rent_module(input$selected_town, input$children_count, input$home_cost, input$other_rental_cost_estimate)
            
            section8_module(earnings, work_supports()[[ "sec8" ]], child_age_hash(), interval, rent_cost_m = calculated_rent, family_size(), nullCheck(outputs_hash[[ "last_received_sec8" ]]), input$disability_parent1, input$disability_parent2, outputs_hash[[ "disability_work_expenses_m" ]], outputs_hash[[ "interest_m" ]], outputs_hash[[ "parent1_earnings" ]], outputs_hash[[ "parent2_earnings" ]], outputs_hash[[ "ssi_recd" ]], outputs_hash[[ "tanf_recd" ]], outputs_hash[[ "child_support_recd" ]], outputs_hash[[ "max_housing_allowance_m" ]], outputs_hash[[ "child_care_expenses" ]], input$selected_town, outputs_hash[[ "health_expenses" ]])
            
            # Assume not eligible for NSLP and children receive no lunch support; also assume that parents and children are all on Medicaid. (See reactive function child_health_hash for where I set all children to "Medicaid.")

            wic_module(work_supports()[[ "wic" ]], child_age_hash(), input$breastfeeding, earnings, fpl = 10000, outputs_hash[[ "interest" ]], outputs_hash[[ "ssi_recd" ]], hlth_cov_parent = "employer", hlth_cov_child = child_health_hash(), outputs_hash[[ "medically_needy" ]], outputs_hash[[ "fsp_recd" ]], outputs_hash[[ "tanf_recd" ]], outputs_hash[[ "child_support_recd" ]], input$parent1_age, input$parent2_age, input$disability_parent1, wic_elig_nslp = 0, child_lunch_red = child_lunch_hash())
            
            fedtax_module(input$family_structure, child_age_hash(), work_supports()[[ "cadc" ]], work_supports()[[ "ctc" ]], input$disability_parent2, input$disability_personal_expenses_m, outputs_hash[[ "parent2_max_hours_w" ]], earnings, outputs_hash[[ "interest" ]], outputs_hash[[ "ssi_recd" ]], outputs_hash[[ "tanf_recd" ]], outputs_hash[[ "child_support_recd" ]], outputs_hash[[ "child_care_expenses" ]], outputs_hash[[ "afterschool_expenses" ]], outputs_hash[[ "fsp_record" ]], outputs_hash[[ "parent1_earnings" ]], outputs_hash[[ "parent2_earnings" ]], outputs_hash[[ "liheap_record" ]], outputs_hash[[ "heap_recd" ]])
            
            eitc_module(work_supports()[[ "eitc" ]], earnings, child_age_hash(), input$family_structure, outputs_hash[[ "interest" ]])
            
            payroll_module(outputs_hash[[ "parent1_earnings" ]], outputs_hash[[ "parent2_earnings" ]])
            
            ctc_module(earnings, child_age_hash(), input$family_structure, work_supports()[[ "ctc" ]], outputs_hash[[ "ctc_reduction" ]], outputs_hash[[ "ctc_potential_red" ]], outputs_hash[[ "federal_tax_cadc" ]], outputs_hash[[ "ctc_nonref_recd" ]], outputs_hash[[ "payroll_tax" ]], eitc_recd = 0, outputs_hash[[ "cadc_recd" ]])

            # Assume no participation in state programs or that parents are NOT incapacitated (need to add these inputs).
            stateandlocaltax_module(earnings, input$heat_in_rent, outputs_hash[[ "disability_personal_expenses_m" ]], state_cadc = 0, state_eitc = 0, state_dec = 0, state_ptfc = 0, state_stfc = 0, parent1_incapacitated == 0, parent2_incapacitated == 0, input$family_structure, length(child_age_hash()), outputs_hash[[ "interest" ]], outputs_hash[[ 'cadc_recd' ]], outputs_hash[[ "cadc_percentage" ]], outputs_hash[[ "filing_status" ]], outputs_hash[[ "federal_tax_gross" ]], eitc_recd = 0, outputs_hash[[ "federal_tax_credits" ]], child_care_expenses = 0, child_care_expenses_regular = 0, child_care_expenses_step4 = 0, outputs_hash[[ "rent_paid" ]], energy_cost = 0)
            
            food_module(input$family_structure, child_age_hash(), input$parent1_age, input$parent2_age, family_size(), input$food_cost, input$other_food_cost, outputs_hash[[ "wic_recd" ]], outputs_hash[[ "child_foodcost_red_total" ]])
            
            lifeline_module(work_supports()[[ "lifeline" ]], earnings, fpl = 10000, hlth_cov_parent = "employer", hlth_cov_children = child_health_hash(), outputs_hash[[ "fsp_recd" ]], outputs_hash[[ "housing_recd" ]], outputs_hash[[ "ssi_recd" ]], outputs_hash[[ "medically_needy" ]])
            
            other_expenses(outputs_hash[[ "rent_paid" ]], outputs_hash[[ "disability_work_expenses_m" ]], outputs_hash[[ "disability_personal_expenses_m" ]], outputs_hash[[ "family_foodcost" ]], outputs_hash[[ "lifeline_recd" ]], length(child_age_hash()), outputs_hash[[ "Other_necessities_cost"]])
                
            salestax_module(outputs_hash[[ "other_expenses" ]])
            
            income <- earnings + outputs_hash[[ "child_support_recd" ]] + outputs_hash[[ "interest" ]] + outputs_hash[[ "tanf_recd "]] +
                outputs_hash[[ "fsp_recd" ]] + outputs_hash[[ "liheap_recd" ]] + outputs_hash[[ "heap_recd" ]] + 
                outputs_hash[[ "federal_tax_credits" ]] + outputs_hash[[ "state_tax_credits" ]] + outputs_hash[[ "local_tax_credits" ]]
            expenses <- outputs_hash[[ "tax_before_credits" ]] + outputs_hash[[ "payroll_tax" ]] + outputs_hash[[ "sales_tax" ]] + 
                outputs_hash[[ "rent_paid" ]] + outputs_hash[[ "child_care_expenses" ]] + outputs_hash[[ "food_expenses" ]] + 
                outputs_hash[[ "trans_expenses" ]] + outputs_hash[[ "other_expenses" ]] + outputs_hash[[ "health_expenses" ]] + 
                outputs_hash[[ "debt_payment" ]]
            
            net_resources <- income - expenses
            
            dat <- data.frame(dollar = earnings, net = net_resources)
            
            datalist[[ (i/1000 + 1) ]] <- dat
        }
        
        resource_table <- do.call(rbind, datalist)
        
        return(resource_table)
        
    })
    
    output$resource_plot <- renderPlot({
        data <- FRS()
        #print(data)
        ggplot(data, aes(dollar, net)) + geom_line(size = 1) +
            xlab("Annual Earnings") + ylab("Net Resources") +
            theme(text=element_text(size=24)) +
            scale_x_continuous(label=dollar_format()) + 
            scale_y_continuous(label=dollar_format()) +
            geom_hline(yintercept = 0, color = "red", size = 1)
    }, height = 800)
        

}

shinyApp(ui = ui, server = server)


