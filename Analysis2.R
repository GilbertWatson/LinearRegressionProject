rm(list=ls())
data <- read.csv("CSV_1242013-33.csv")

#remove id variables
ids <- c(
"HD2011.Data.Feedback.Report.comparison.group.category.created.by.NCES", 
"HD2011.Data.Feedback.Report.comparison.group.category.created.by.NCES",
"HD2011.Postsecondary.and.Title.IV.institution.indicator",
"HD2011.Degree.granting.status",
"year",
"institution.name",
"unitid",
"HD2011.State.abbreviation",
"HD2011.Level.of.institution",
"HD2011.Data.Feedback.Report...Institution.submitted.a..custom.comparison.group",
"HD2011.Tribal.college",
"XSTUFACR")

#remove response codes
response <- names(data)[grep("^X",names(data))]

#add ids to rows
row.names(data) <- paste0(as.character(data$institution.name)," : ",as.character(data$HD2011.FIPS.state.code))
#remove ids and gradrates
data <- data[,setdiff(names(data),c(ids,response))]

#remove for profit institutions
data <- data[(data$HD2011.Sector.of.institution != "Private for-profit, 4-year or above"),]

#clean financial variables
mergeGASBFASB <- function(GASB) {
  start <- data[,c(GASB)]
  try(fasb <- data[,gsub("GASB","FASB",GASB)],silent=T)
  data <- start
  try(data[(is.na(data))] <- fasb[(is.na(data))],silent=T)
  return(data)
}

newfinancials <- as.data.frame(sapply(names(data)[grep("..GASB.",names(data),fixed=T)],mergeGASBFASB))
names(newfinancials) <- gsub("..GASB.","..MERGE",names(newfinancials),fixed=T)

#remove old financials from dataset
data <- data[,setdiff(names(data),names(data)[grepl("(\\.\\.GASB\\.)|(\\.\\.FASB\\.)",names(data))])]

#add new financials
data <- cbind(data,newfinancials)

#tution data for institutions without in-state, out of state distiction
data$DRVIC2011_RV.Total.price.for.in.district.students.living.on.campus..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.district.students.living.on.campus..2011.12)] <- data$DRVIC2011_RV.Tuition.and.fees..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.district.students.living.on.campus..2011.12)]                                                                             
data$DRVIC2011_RV.Total.price.for.in.state.students.living.on.campus.2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.state.students.living.on.campus.2011.12)] <- data$DRVIC2011_RV.Tuition.and.fees..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.state.students.living.on.campus.2011.12)]                                                                                  
data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.on.campus.2011.12[is.na(data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.on.campus.2011.12)] <- data$DRVIC2011_RV.Tuition.and.fees..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.on.campus.2011.12)]                                                                              
data$DRVIC2011_RV.Total.price.for.in.district.students.living.off.campus..not.with.family...2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.district.students.living.off.campus..not.with.family...2011.12)] <- data$DRVIC2011_RV.Tuition.and.fees..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.district.students.living.off.campus..not.with.family...2011.12)]                                                           
data$DRVIC2011_RV.Total.price.for.in.state.students.living.off.campus..not.with.family...2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.state.students.living.off.campus..not.with.family...2011.12)] <- data$DRVIC2011_RV.Tuition.and.fees..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.state.students.living.off.campus..not.with.family...2011.12)]                                                              
data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..not.with.family...2011.12[is.na(data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..not.with.family...2011.12)] <- data$DRVIC2011_RV.Tuition.and.fees..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..not.with.family...2011.12)]                                                          
data$DRVIC2011_RV.Total.price.for.in.district.students.living.off.campus..with.family...2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.district.students.living.off.campus..with.family...2011.12)] <- data$DRVIC2011_RV.Tuition.and.fees..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.district.students.living.off.campus..with.family...2011.12)]                                                              
data$DRVIC2011_RV.Total.price.for.in.state.students.living.off.campus..with.family...2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.state.students.living.off.campus..with.family...2011.12)] <- data$DRVIC2011_RV.Tuition.and.fees..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.in.state.students.living.off.campus..with.family...2011.12)]                                                                
data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..with.family...2011.12[is.na(data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..with.family...2011.12)] <- data$DRVIC2011_RV.Tuition.and.fees..2011.12[is.na(data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..with.family...2011.12)]

#graduate enrollment pcts for schools with no graduate enrollment
data$anyGrads <- sapply(data$DRVEF2011_RV.Graduate.enrollment,function(x) {
  if (x > 0) {
    return(1)
  }
  if (x == 0) {
    return(0)
  }
})

#make all graduate percentages interaction terms
data[,c("DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.American.Indian.or.Alaska.Native",                                                                     
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Asian",                                                                                                
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Black.or.African.American",                                                                            
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Hispanic.Latino",                                                                                      
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Native.Hawaiian.or.Other.Pacific.Islander",                                                            
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.White",                                                                                                
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.two.or.more.races",                                                                                    
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Race.ethnicity.unknown",                                                                               
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Nonresident.Alien",                                                                                    
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Asian.Native.Hawaiian.Pacific.Islander",                                                               
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.women")] <- apply(
  data[,c("DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.American.Indian.or.Alaska.Native",                                                                     
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Asian",                                                                                                
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Black.or.African.American",                                                                            
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Hispanic.Latino",                                                                                      
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Native.Hawaiian.or.Other.Pacific.Islander",                                                            
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.White",                                                                                                
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.two.or.more.races",                                                                                    
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Race.ethnicity.unknown",                                                                               
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Nonresident.Alien",                                                                                    
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.Asian.Native.Hawaiian.Pacific.Islander",                                                               
"DRVEF2011_RV.Percent.of.graduate.enrollment.that.are.women")],
  c(1,2),
  function(x) {
    if(is.na(x)) {
      return(0)
    }
    else {
      return(x)
    }
  })

#create a variable for open admissions schools or schools that don't admit a gender
openAdmission <- is.na(data$DRVIC2011_RV.Percent.admitted...total)
MenOnly <- !is.na(data$DRVIC2011_RV.Percent.admitted...total) & is.na(data$DRVIC2011_RV.Percent.admitted...women) & !is.na(data$DRVIC2011_RV.Percent.admitted...men)
WomenOnly <- !is.na(data$DRVIC2011_RV.Percent.admitted...total) & !is.na(data$DRVIC2011_RV.Percent.admitted...women) & is.na(data$DRVIC2011_RV.Percent.admitted...men)
data$AdmitsGenders <- apply(cbind(MenOnly,WomenOnly,openAdmission),1,function(x) {
  if (x[3] == T) {
    return("Open Admissions - No determination")
  }
  else {
    if (any(x[1],x[2])) {
    if (x[1] == T) {
      return("Men Only")
    }
    if (x[2] == T) {
      return("Women Only")
    }
  }
  else {
    return("All Genders")
  }
  }
})

#make everything an interaction term based upon admitting all genders and having selective admissions (binary variables )
#data$DRVIC2011_RV.Percent.admitted...total[openAdmission] <- 100 #make percent admitted to open admissions 100 to indicate little selectivity
data$DRVIC2011_RV.Percent.admitted...men[openAdmission|WomenOnly] <- 0
data$DRVIC2011_RV.Percent.admitted...women[openAdmission|MenOnly] <- 0
data$DRVIC2011_RV.Admissions.yield...total[openAdmission] <- 0
data$DRVIC2011_RV.Admissions.yield...men[openAdmission|WomenOnly] <- 0
data$DRVIC2011_RV.Admissions.yield...women[openAdmission|MenOnly] <- 0
data$DRVIC2011_RV.Admissions.yield...full.time[openAdmission] <- 0
data$DRVIC2011_RV.Admissions.yield...full.time.men[openAdmission|WomenOnly] <- 0
data$DRVIC2011_RV.Admissions.yield...full.time.women[openAdmission|MenOnly] <- 0
data$DRVIC2011_RV.Admissions.yield...part.time[openAdmission] <- 0
data$DRVIC2011_RV.Admissions.yield...part.time.men[openAdmission|WomenOnly] <- 0
data$DRVIC2011_RV.Admissions.yield...part.time.women[openAdmission|MenOnly] <- 0

#derive some variables with care taken not to introduce linear dependencies
derived <- NULL

#men women admission and yeild
derived$men.women.admitted.differential <- data$DRVIC2011_RV.Percent.admitted...men - data$DRVIC2011_RV.Percent.admitted...women
derived$men.women.admission.yeild.differential <- data$DRVIC2011_RV.Admissions.yield...men - data$DRVIC2011_RV.Admissions.yield...women
derived$men.women.admission.yeild.full.time.differential <- data$DRVIC2011_RV.Admissions.yield...full.time.men - data$DRVIC2011_RV.Admissions.yield...full.time.women
derived$men.women.admission.yeild.part.time.differential <- data$DRVIC2011_RV.Admissions.yield...part.time.men - data$DRVIC2011_RV.Admissions.yield...full.time.women
derived$full.part.admission.yeild.differential <- data$DRVIC2011_RV.Admissions.yield...full.time - data$DRVIC2011_RV.Admissions.yield...part.time

#fees
derived$tuition.change.2008.2011 <- (data$DRVIC2011_RV.Tuition.and.fees..2010.11 - data$DRVIC2011_RV.Tuition.and.fees..2008.09)/data$DRVIC2011_RV.Tuition.and.fees..2008.09
derived$tuition.change.2009.2010 <- (data$DRVIC2011_RV.Tuition.and.fees..2010.11 - data$DRVIC2011_RV.Tuition.and.fees..2009.10)/data$DRVIC2011_RV.Tuition.and.fees..2009.10
derived$in.state.out.state.on.campus.differential <- (data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.on.campus.2011.12 - data$DRVIC2011_RV.Total.price.for.in.state.students.living.on.campus.2011.12)/data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.on.campus.2011.12
derived$in.state.out.state.off.campus.family.differential <- (data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..with.family...2011.12 - data$DRVIC2011_RV.Total.price.for.in.state.students.living.off.campus..with.family...2011.12)/data$DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..not.with.family...2011.12

#enrollment
derived$percent.full.time.enrollment <- data$DRVEF2011_RV.Full.time.enrollment/data$DRVEF2011_RV.Total..enrollment
derived$percent.undergraduate.enrollment <- data$DRVEF2011_RV.Undergraduate.enrollment/data$DRVEF2011_RV.Total..enrollment
derived$percent.first.time.undergraduate.enrollment <- data$DRVEF2011_RV.First.time.degree.certificate.seeking.undergraduate.enrollment/data$DRVEF2011_RV.Total..enrollment
derived$percent.first.time.transfer.ugrad.enrollment <- data$DRVEF2011_RV.Transfer.in.degree.certificate.seeking.undergraduate.enrollment/data$DRVEF2011_RV.Total..enrollment
derived$percent.first.time.continuing.ugrad.degree.enrollment <- data$DRVEF2011_RV.Continuing.degree.certificate.seeking.undergraduate.enrollment/data$DRVEF2011_RV.Total..enrollment
derived$percent.full.time.undergrad.enrollment <- data$DRVEF2011_RV.Full.time.undergraduate.enrollment/data$DRVEF2011_RV.Total..enrollment
derived$percent.first.time.full.time.ugrad.enrollment <- data$DRVEF2011_RV.Full.time.first.time.degree.certificate.seeking.undergraduate.enrollment/data$DRVEF2011_RV.Total..enrollment
derived$percent.first.time.full.time.ugrads.of.all.ugrads <- data$DRVEF2011_RV.Full.time.first.time.degree.certificate.seeking.undergraduate.enrollment/data$DRVEF2011_RV.Undergraduate.enrollment

#staff
derived$percent.instruction.research.public.staff <- data$DRVHR2011_RV.Instruction.research.and.public.service.FTE.staff/data$DRVHR2011_RV.Total.FTE.staff
derived$percent.admin.staff <- data$DRVHR2011_RV.Executive.administrative.and.managerial.FTE..staff/data$DRVHR2011_RV.Total.FTE.staff
derived$percent.nonprofessional.staff <- data$DRVHR2011_RV.Non.professional.FTE.staff/data$DRVHR2011_RV.Total.FTE.staff

#salary differentials
derived$percent.prof.salary <- (data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...professors - data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks)/data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks
derived$percent.associate.salary <- (data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...associate.professors - data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks)/data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks
derived$percent.assistant.salary <- (data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...assistant.professors - data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks)/data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks
derived$percent.instructor.salary <- (data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...instructors - data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks)/data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks
derived$percent.lecturer.salary <- (data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...lecturers - data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks)/data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks
derived$percent.no.academic.rank.salary <- (data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...No.academic.rank - data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks)/data$DRVHR2011_RV.Average.salary.equated.to.9.month.contracts.of.full.time.instructional.staff...all.ranks

#aid
derived$percent.average.aid.total.tuition <- data$SFA1011_RV.Average.amount.of.federal..state..local.or.institutional.grant.aid.received/data$DRVIC2011_RV.Tuition.and.fees..2008.09
derived$percent.average.aid.total.tuition.first.time.ugrad <- data$SFA1011_RV.Average.amount.of.federal.grant.aid.received.by.full.time.first.time.undergraduates/data$DRVIC2011_RV.Tuition.and.fees..2008.09
derived$percent.average.student.loan.tuition.first.time.ugrad <- data$SFA1011_RV.Average.amount.of.student.loan.aid.received.by.full.time.first.time.undergraduates/data$DRVIC2011_RV.Tuition.and.fees..2008.09
derived$percent.average.federal.grant.tuition.first.time.ugrad <- data$SFA1011_RV.Average.amount.of.other.federal.grant.aid.received.by.full.time.first.time.undergraduates/data$DRVIC2011_RV.Tuition.and.fees..2008.09
derived$percent.average.federal.loan.tuition.first.time.ugrad <- data$SFA1011_RV.Average.amount.of.federal.student.loan.aid.received.by.full.time.first.time.undergraduates/data$DRVIC2011_RV.Tuition.and.fees..2008.09
derived$percent.average.instition.aid.first.time.ugrad <- data$SFA1011_RV.Average.amount.of.institutional.grant.aid.received.by.full.time.first.time.undergraduates/data$DRVIC2011_RV.Tuition.and.fees..2008.09
derived$percent.average.state.local.aid.first.time.ugrad <- data$SFA1011_RV.Average.amount.of.state.local.grant.aid.received.by.full.time.first.time.undergraduates/data$DRVIC2011_RV.Tuition.and.fees..2008.09

derived <- as.data.frame(derived)[!is.na(data$DRVIC2011_RV.Tuition.and.fees..2010.11),]

#remove responses from data and observations with no response
responsevars <- c("DRVIC2011_RV.Tuition.and.fees..2008.09","DRVIC2011_RV.Tuition.and.fees..2009.10","DRVIC2011_RV.Tuition.and.fees..2011.12",
                  "DRVIC2011_RV.Total.price.for.in.district.students.living.on.campus..2011.12",
                  "DRVIC2011_RV.Total.price.for.out.of.state.students.living.on.campus.2011.12",
                  "DRVIC2011_RV.Total.price.for.in.state.students.living.off.campus..not.with.family...2011.12",
                  "DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..not.with.family...2011.12",
                  "DRVIC2011_RV.Total.price.for.in.state.students.living.off.campus..with.family...2011.12",
                  "DRVIC2011_RV.Total.price.for.out.of.state.students.living.off.campus..with.family...2011.12",
                  "DRVIC2011_RV.Total.price.for.in.state.students.living.on.campus.2011.12",
                  "DRVF2011_RV.Core.revenues..total.dollars..MERGE",
                  "DRVF2011_RV.Revenues.from.tuition.and.fees.per.FTE..MERGE",
                  "DRVF2011_RV.Tuition.and.fees.as.a.percent.of.core.revenues..MERGE")
data <- data[!is.na(data$DRVIC2011_RV.Tuition.and.fees..2010.11),setdiff(names(data),responsevars)]

#remove single factors
test <- apply(data,2,unique)
test2 <- unlist(lapply(test,length))
singlefactors <- names(which(test2 == 1))
data <- data[,setdiff(names(data),singlefactors)]

#missing data assessment
missing <- apply(data,2,function(x) {return(sum(is.na(x)))})

#everything has an interaction with response, just so we can assess possible correlations in the data
data.tmp <- data

#list other variables that have little meaning for some observations (specific to survey, not to observational units)
others <- c("DRVEF2011_RV.Full.time..first.time..degree.certificate.seeking.undergraduates..GRS.Cohort..as.percent.of.all.undergraduates",
            "EF2011D_RV.Current.year.GRS.cohort.as.a.percent.of.entering.class",
            "HD2011.Institution.size.category.1")

#remove variables that cause partitions in the data, leading to linear dependencies
lindens <- c("DRVEF2011_RV.Adult.age..25.64..enrollment..full.time.graduate",
             "DRVEF2011_RV.Adult.age..25.64..enrollment..part.time.students",
             "DRVEF2011_RV.Adult.age..25.64..enrollment..part.time.graduate",
             "HD2011.Control.of.institution",
             "DRVEF2011_RV.Part.time.enrollment",
             "DRVEF2011_RV.Graduate.enrollment",
             "DRVEF2011_RV.Part.time.undergraduate.enrollment",
             "DRVEF2011_RV.Adult.age..25.64..enrollment..graduate",
             "DRVEF2011_RV.Adult.age..25.64..enrollment..part.time.undergraduate",
             "DRVEF2011_RV.Part.time.graduate.enrollment",
             "DRVEF2011_RV.Full.time.graduate.enrollment",
             "DRVIC2011_RV.Admissions.yield...part.time",
             "DRVEF2011_RV.Nondegree.certificate.seeking.undergraduate.enrollment",
             "DRVEF2011_RV.Full.time.nondegree.certificate.seeking.undergraduate.enrollment",
             "DRVEF2011_RV.Part.time.nondegree.certificate.seeking.undergraduate.enrollment",
             "DRVIC2011_RV.Total.price.for.in.district.students.living.off.campus..not.with.family...2011.12",
             "DRVIC2011_RV.Total.price.for.in.district.students.living.off.campus..with.family...2011.12",
             "DRVEF2011_RV.Continuing.degree.certificate.seeking.undergraduate.enrollment",
             "DRVEF2011_RV.Full.time.continuing.degree.certificate.seeking.undergraduate.enrollment",
             "DRVEF2011_RV.Part.time.continuing.degree.certificate.seeking.undergraduate.enrollment",
             "DRVEF2011_RV.Part.time.first.time.degree.certificate.seeking.undergraduate.enrollment",
             "DRVEF2011_RV.Part.time.transfer.in.degree.certificate.seeking.undergraduate.enrollment",
             "DRVIC2011_RV.Admissions.yield...part.time.men",
             "DRVIC2011_RV.Admissions.yield...part.time.women",
             "DRVIC2011_RV.Admissions.yield...full.time.men",
             "DRVIC2011_RV.Admissions.yield...full.time.women",
             "DRVIC2011_RV.Admissions.yield...men",
             "DRVIC2011_RV.Percent.admitted...men",
             "HD2011.FIPS.state.code") #creates linear dependencies with region codes, so let's start there first

#remove widely missing variables from consideration
widelymissing <- names(missing)[missing > 100]

# replace infinte values (rates generated by dividing by zero) with zero (temporary matrix)
derived.tmp <- as.data.frame(apply(derived, 1:2, function(x) {
  if (is.infinite(x)) {
    return(0)
  }
  else {
    return(x)
  }
} ))

# peice it all together with temporary data
data.tmp <- cbind(data[,setdiff(names(data),c(others,lindens,widelymissing))],derived.tmp) 
widelymissing2 <- apply(data.tmp,2,function(x) {return(sum(is.na(x)))})
widelymissing2 <- names(widelymissing2)[widelymissing2 > 100]
data.tmp <- data.tmp[,setdiff(names(data.tmp),widelymissing2)]
#write to file
if(!file.exists("CleanData2.csv")) {
  write.csv(data.tmp,file="CleanData2.csv",row.names=T)
}

fullmodel <- lm(DRVIC2011_RV.Tuition.and.fees..2010.11~.,data=data.tmp)
fmzeros_archive <- fullmodel
xx<-model.matrix(fullmodel)[,-1]

# #confirm that model matrix is full rank and that all linear dependencies have been removed 
# rankifremoved <- sapply(1:ncol(xx), function (x) qr(xx[,-x])$rank) #find which removals make matrix full rank
# xm <- xx[,-which(rankifremoved == max(rankifremoved))] #should have nothing
# xo <- xx[,which(rankifremoved == max(rankifremoved))] #should have everything

#do automated variable selection both forwards and backwards in an attempt to find a starting model, get a liberal number of variables
#exhaustive search takes a long long time with this many variables.
library(MASS)
library(leaps)
modelsf <- regsubsets(x=xx,nbest=20,nvmax=50,y=fullmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11,method="forward",really.big=T)
modelsb <- regsubsets(x=xx,nbest=20,nvmax=50,y=fullmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11,method="backward",really.big=T)
modelss <- regsubsets(x=xx,nbest=20,nvmax=50,y=fullmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11,method="seqrep",really.big=T)


# get best model from both forward, backwards, and sequential deletions
whichmodelf <- summary(modelsf)$which[summary(modelsf)$bic == min(summary(modelsf)$bic),]
whichmodelb <- summary(modelsb)$which[summary(modelsb)$bic == min(summary(modelsb)$bic),]
whichmodels <- summary(modelss)$which[summary(modelss)$bic == min(summary(modelss)$bic),]
inatleastone <- whichmodelf | whichmodelb | whichmodels

testmodel <- lm(fullmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11~xx[,which(inatleastone[2:length(inatleastone)] == T)])

#everything has an interaction with response, just so we can assess possible correlations in the data
data.tmp <- data

# replace infinte values (rates generated by dividing by zero) with zero (temporary matrix)
derived.tmp <- as.data.frame(apply(derived, 1:2, function(x) {
  if (is.infinite(x)) {
    return(0)
  }
  else {
    return(x)
  }
} ))

# peice it all together with temporary data
data.tmp <- cbind(data[,setdiff(names(data),c(others,lindens))],derived.tmp)

#get a list of the variables suggested from the steps above
varsin <- attr(inatleastone,"names")[(inatleastone == T)]

# add the factors back carefully (hand process)
facs <- c("HD2011.Institution.size.category",
          "HD2011.Geographic.region",
          "HD2011.Sector.of.institution",
          "HD2011.Degree.of.urbanization..Urban.centric.locale.",
          "HD2011.Carnegie.Classification.2010..Undergraduate.Instructional.Program",
          "HD2011.Carnegie.Classification.2010..Graduate.Instructional.Program",
          "HD2011.Carnegie.Classification.2010..Undergraduate.Profile",
          "HD2011.Carnegie.Classification.2000",
          "anyGrads")

# and of course the response variable
resp <- c("DRVIC2011_RV.Tuition.and.fees..2010.11")

# get just the data we want in a matrix, but make sure we don't include variables with a high degree of missingness (since we modeled with 0's before)
data.m <- data.tmp[,names(data.tmp)[names(data.tmp) %in% setdiff(c(varsin,facs,resp),names(missing)[missing > 100])]]

# estimate the new model with these variables, getting rid of missing data in the process
fullmodel <- lm(DRVIC2011_RV.Tuition.and.fees..2010.11~.,data=data.m)
xx<-model.matrix(fullmodel)[,-1]

#do automated variable selection both forwards and backwards in an attempt to find a starting model, get a liberal number of variables
#exhaustive search takes a long long time with this many variables.
modelsf <- regsubsets(x=xx,nbest=20,nvmax=21,y=fullmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11,method="forward",really.big=T)
modelsb <- regsubsets(x=xx,nbest=20,nvmax=21,y=fullmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11,method="backward",really.big=T)
modelss <- regsubsets(x=xx,nbest=20,nvmax=21,y=fullmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11,method="seqrep",really.big=T)


# get best model from both forward, backwards, and sequential deletions
whichmodelf <- summary(modelsf)$which[summary(modelsf)$bic == min(summary(modelsf)$bic),]
whichmodelb <- summary(modelsb)$which[summary(modelsb)$bic == min(summary(modelsb)$bic),]
whichmodels <- summary(modelss)$which[summary(modelss)$bic == min(summary(modelss)$bic),]
inatleastone <- whichmodelf | whichmodelb | whichmodels

#create a new dataset to do exhaustive search on
testdata <- as.data.frame(cbind(fullmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11,xx[,which(inatleastone[2:length(inatleastone)] == T)]))
names(testdata)[1] <- "DRVIC2011_RV.Tuition.and.fees..2010.11"
testmodel <- lm(DRVIC2011_RV.Tuition.and.fees..2010.11~.,data=testdata)
xx <- model.matrix(testmodel)[,-1]

#branch and prune selection
models <- leaps(x=xx,y=testmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11,method="adjr",nbest=3)

#estimate the best model
lastdata <- as.data.frame(cbind(testmodel$model$DRVIC2011_RV.Tuition.and.fees..2010.11,xx[,which(models$which[(models$adjr2 == max(models$adjr2)),])]))
names(lastdata)[1] <- "DRVIC2011_RV.Tuition.and.fees..2010.11"
lastmodel <- lm(DRVIC2011_RV.Tuition.and.fees..2010.11~.,data=lastdata)
summary(lastmodel)

#write minimal data to file
if(!file.exists("MinimalData2.csv")) {
  write.csv(data.m,file="MinimalData2.csv",row.names=T)
}

#rename testdata
names(lastdata) <- c("Tuition.Fees.2010.2011",
                     "South.East",
                     "Public",
                     "Ugrad.Instr.Profile.Profession.Arts.Science.NoGraduateCoexist",
                     "Ugrad.Instr.Profile.Profession.Arts.Science.SomeGraduateCoexist",
                     "UgradProfile.FullTime.MoreSelective.LowTransfer",
                     "Percent.Ugrad.African.American",
                     "Percent.Ugrad.Hispanic",
                     "Percent.Full.Time.First.Time.Ugrads.Recieving.FedStateLocalInst.Aid",
                     "Percent.Full.Time.First.Time.Ugrads.Recieving.Pell.Grants",
                     "Percent.Full.Time.First.Time.Ugrads.Recieving.StateLocal.Aid",
                     "Average.State.Local.Grant.To.First.Time.Full.Time.Ugrads",
                     "Percent.Full.Time.First.Time.Ugrads.Recieving.Inst.Aid",
                     "Average.Inst.Aid.To.First.Time.Full.Time.Ugrads",
                     "Percent.Full.Time.First.Time.Ugrads.Recieving.Loan.Aid",
                     "Average.Loan.Aid.To.First.Time.Full.Time.Ugrads",
                     "Average.Federal.Aid.To.First.Time.First.Time.Ugrads",
                     "Other.Professional.FTE.Staff",
                     "Average.Salary.Professors",
                     "Average.Salary.Assistant.Professors",
                     "Endowment.Per.FTE.Enrollment",
                     "Full.Time.Part.Time.Admission.Yeild.Differential",
                     "Tuition.Change.2009.2010",
                     "In.State.Out.Of.State.Enrollment.Differential",
                     "Percent.First.Time.Transfer.Ugrad.Enrollment",
                     "Percent.Average.Aid.Of.Tuition",
                     "Percent.Average.Student.Loan.Of.Tuition.First.Time.Ugrad",
                     "Percent.Average.Federal.Loan.Of.Tuition.First.Time.Ugrad",
                     "Percent.Average.Institution.Aid.Of.Tuition.First.Time.Ugrad",
                     "Percent.Average.State.Local.Aid.Of.Tuition.First.Time.Ugrad")
GradRate2 <- lastdata
lastmodel2 <- lastmodel

save(GradRate2,file="GradRate2.RData")
save(lastmodel2,file="lastmodel2.RData")

