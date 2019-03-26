library(readxl)
library(kableExtra)
library(dplyr)
## 0- Loading the data
data <- data.frame(read_xlsx('Data/D Mining Sample NA.xlsx'))

## 1- Delete stars in char type variables and convert them to numeric.
# Delete star function
delete_star_f <- function(x) if (is.na(x) == F) return(as.numeric(strsplit(x, split = " ")[[1]][1])) else return(as.numeric(x))
# Find char variables
char_vars <- sapply(c(1:ncol(data)), function(x) is.character(data[, x]))
# Delete stars and convert to numeric
data[, char_vars] <- sapply(data[, char_vars], function(x) sapply(x, function(y) delete_star_f(y)))
data <- data.frame(data)

## 2- CBC Bug 
i <- which(colnames(data) == 'WBC')
j <- which(colnames(data) == 'MPV')
left <- which(!is.na(data$CBC) & data$CBC < 1000)
right <- which(!is.na(data$CBC) & data$CBC > 1000)
#shift to left
data[left, c(i:(j-1))] <- data[left, c((i+1):j)]
data[left, 'CBC'] <- NA
#shift to right
data[right, c(i:j)] <- data[right, c((i-1):(j-1))]
data[right, 'CBC'] <- NA

## 3- change FesharD and FesharS when it's reverse
ind <- which(data$FesharS < data$FesharD)
fs <- data[ind, 'FesharS']
data[ind, 'FesharS'] <- data[ind, 'FesharD']
data[ind, 'FesharD'] <- fs

## 4- Replacing zeros in 'test' variables with NA. 
# Find test variables
tst_vars <- sapply(c(1:ncol(data)), function(x) "test" == substr(colnames(data)[x], 1, 4))
# Replace zero with NA
data[, tst_vars] <- sapply(data[, tst_vars], function(x) replace(x, which(x == 0), NA))
data <- data.frame(data)


## 5- Set wrong values in tests (values not in the choices) to null
two_option_tests <- c(2,4, 62, 66:71, 74:75, 77:80, 83, 88:89, 92:93, 96:98, 101:102, 106:107, 110:111, 
                      114:115, 117, 119:121, 125:126, 129:130, 133:134, 137:138, 141:142, 145:146, 149:150,
                      153:154, 157:158, 160, 162, 164, 166, 168, 170:173, 182, 231:232, 242, 249, 251, 262:263,
                      267:268, 272:273)

three_option_tests <- c(86, 181, 233:234, 258, 260)

four_option_tests <- c(5, 8, 20, 22:26, 33:53, 103, 187, 193, 196, 244, 252:257, 259, 261, 264:266, 269:271)

five_option_tests <- c(1, 3, 6, 7, 9:19, 21, 27:32, 54:61, 63:65, 72, 73, 76, 81:82, 84:85, 87, 90:91, 94:95, 
                       99:100, 104:105, 108:109, 112:113, 116, 118, 122:124, 127:128, 131:132, 135:136, 139:140,
                       143:144, 147:148, 151:152, 155:156, 159, 161, 163, 165, 167, 169, 174:180, 183:186,
                       188:192, 194:195, 197:230, 235:241, 243, 245:248, 250)

two_option_tests <- sapply(two_option_tests, function(x) paste("test", x, sep = ""))
two_option_tests_indices <- sapply(two_option_tests, function(x) match(x, colnames(data)))
two_option_tests_indices <- two_option_tests_indices[!is.na(two_option_tests_indices)]

three_option_tests <- sapply(three_option_tests, function(x) paste("test", x, sep = ""))
three_option_tests_indices <- sapply(three_option_tests, function(x) match(x, colnames(data)))
three_option_tests_indices <- three_option_tests_indices[!is.na(three_option_tests_indices)]

four_option_tests <- sapply(four_option_tests, function(x) paste("test", x, sep = ""))
four_option_tests_indices <- sapply(four_option_tests, function(x) match(x, colnames(data)))
four_option_tests_indices <- four_option_tests_indices[!is.na(four_option_tests_indices)]

five_option_tests <- sapply(five_option_tests, function(x) paste("test", x, sep = ""))
five_option_tests_indices <- sapply(five_option_tests, function(x) match(x, colnames(data)))
five_option_tests_indices <- five_option_tests_indices[!is.na(five_option_tests_indices)]

data[, two_option_tests_indices] <- sapply(data[, two_option_tests_indices], function(x) replace(x, which(x > 2), NA))
data[, three_option_tests_indices] <- sapply(data[, three_option_tests_indices], function(x) replace(x, which(x > 3), NA))
data[, four_option_tests_indices] <- sapply(data[, four_option_tests_indices], function(x) replace(x, which(x > 4), NA))
data[, five_option_tests_indices] <- sapply(data[, five_option_tests_indices], function(x) replace(x, which(x > 5), NA))

## 6- Delete continuous outliers
# Ranges
ranges <- list()
ranges$Vazn <- c(5, 200)
ranges$Ghad <- c(80, 220)
ranges$BMI <- c(9, 60)
ranges$MetaP <- c(750, 5000)
ranges$CharbiE <- c(.1, 39)
ranges$FesharS <- c(60, 250)
ranges$FesharD <- c(30, 250)
ranges$Nabz <- c(30, 200)
ranges$DarsadChB <- c(1, 100)
ranges$DarsadAzB <- c(1, 100)
ranges$DorBadan <- c(30, 200)
ranges$DorBasan <- c(30, 200)
ranges$DorGardan <- c(15, 100)
ranges$FSG <- c(40, 400)
ranges$Urea <- c(5, 50)
ranges$UAC <- c(.1, 10)
ranges$Chol <- c(80, 400)
ranges$TG <- c(20, 1000)
ranges$Crea <- c(.2, 4)
ranges$LDH <- c(10, 100)
ranges$WBC <- c(1500, 20000)
ranges$RBC <- c(3, 20)
ranges$HB <- c(5, 20)
ranges$Hct <- c(20, 60)
ranges$MCV <- c(60, 102)
ranges$MCH <- c(15, 50)
ranges$MCHC <- c(25, 40)
ranges$Platelet <- c(50000, 750000)
ranges$Lymph <- c(10, 80)
ranges$Mxd <- c(2, 25)
ranges$Neut <- c(15, 90)
ranges$RDW <- c(5, 30)
ranges$PDW <- c(8, 40)
ranges$MPV <- c(5, 15)
# Delete outliers
data[, names(ranges)] <- sapply(names(ranges), function(x) {data[which(data[, x] < ranges[[x]][1] | data[, x] > ranges[[x]][2]), x] <- NA
return(data[, x])})

# ## Specify family tests
# family_tests <- c(70, 74, 77, 88, 92, 97, 101, 106, 110, 114, 120, 125, 133, 137, 141, 145, 149, 153, 157)
# family_tests <- sapply(family_tests, function(x) paste("test", x, sep = ""))
# ## Specify tests related to a subset of data
# specific_tests <- list()
# add_spec_test <- function(name, tests, specific_tests) {
#   list_names <- c(names(specific_tests), name)
#   specific_tests[[length(specific_tests) + 1]] <- tests
#   specific_tests <- setNames(specific_tests, list_names)
#   return (specific_tests)
# }
# specific_tests <- add_spec_test("yazd_native", c(5), specific_tests)
# specific_tests <- add_spec_test("heart_disease", c(63:69), specific_tests)
# specific_tests <- add_spec_test("hypertension", c(72:73), specific_tests)
# specific_tests <- add_spec_test("mental_ill", c(76), specific_tests)
# specific_tests <- add_spec_test("diabetes", c(81:87), specific_tests)
# specific_tests <- add_spec_test("increased_blood_chol", c(90:91), specific_tests)
# specific_tests <- add_spec_test("blood_coagulation_prob", c(94:96), specific_tests)
# specific_tests <- add_spec_test("asthma", c(99:100), specific_tests)
# specific_tests <- add_spec_test("thyroid_prob", c(103:105), specific_tests)
# specific_tests <- add_spec_test("depression", c(108, 109), specific_tests)
# specific_tests <- add_spec_test("alzheimer", c(112, 113), specific_tests)
# specific_tests <- add_spec_test("osteoporosis", c(116:119), specific_tests)
# specific_tests <- add_spec_test("joint_pain", c(122:124), specific_tests)
# specific_tests <- add_spec_test("skin_cancer", c(127:128), specific_tests)
# specific_tests <- add_spec_test("breast_cancer", c(130:132), specific_tests)
# specific_tests <- add_spec_test("colon_cancer", c(139:140), specific_tests)
# specific_tests <- add_spec_test("stomach_cancer", c(143:144), specific_tests)
# specific_tests <- add_spec_test("lung_cancer", c(135:136), specific_tests)
# specific_tests <- add_spec_test("colorectal_cancer", c(147:148), specific_tests)
# specific_tests <- add_spec_test("prostate_cancer", c(150:152), specific_tests)
# specific_tests <- add_spec_test("ovarian_cancer", c(154:156), specific_tests)
# specific_tests <- add_spec_test("bladder_surg", c(159), specific_tests)
# specific_tests <- add_spec_test("kidney_stone_surg", c(161), specific_tests)
# specific_tests <- add_spec_test("knee_replac_surg", c(163), specific_tests)
# specific_tests <- add_spec_test("hip_surg", c(165), specific_tests)
# specific_tests <- add_spec_test("gallbladder_surg", c(167), specific_tests)
# specific_tests <- add_spec_test("open_hard_surg", c(169), specific_tests)
# specific_tests <- add_spec_test("accident", c(187:194), specific_tests)
# specific_tests <- add_spec_test("med_use", c(231), specific_tests)
# specific_tests <- add_spec_test("employee", c(235:236), specific_tests)
# specific_tests <- add_spec_test("trad_practice", c(243:252), specific_tests)
# specific_tests <- add_spec_test("smoke", c(254:257), specific_tests)
# specific_tests <- add_spec_test("hookah", c(259), specific_tests)
# specific_tests <- add_spec_test("drug", c(261,262), specific_tests)
# specific_tests <- add_spec_test("woman", c(274:300), specific_tests)
# 
# spec_tests <- do.call(c, specific_tests)
# spec_tests <- sapply(spec_tests, function(x) paste("test", x, sep = ""))
# spec_test_indices <- sapply(spec_tests, function(x) match(x, colnames(data)))
# spec_test_indices <- spec_test_indices[!is.na(spec_test_indices)]
# ## Replacing NA with 99 in these tests
# # data[, spec_test_indices] <- sapply(data[, spec_test_indices], function(x) replace(x, which(is.na(x)), 99))


## 7- Delete rows with more than 20 NA values in continuous variables
cont_var_number <- which(colnames(data) == 'test1') - 1
data <- data[apply(data[, 1:cont_var_number], 1, function(x) sum(is.na(x)) < 20), ]

## 8- Delete columns with more than .4 NA values
data <- data[, colMeans(is.na(data)) < .4]


## 9- Preprocess using data_preproc
proc_data <- data_preproc(data, level = 5, detect.outliers = F, alpha = .2)

## 10- Change names
new_names <- c('Weight', 'Height', 'BMI', 'MetaP', 'Fat', 'BloodPreS', 'BloodPreD', 'Pulse', 'FatPerc', 'MusclePerc', 'WaistCircum', 'HipCircum', 'NeckCircum')
for (i in c(1:13)) {
  colnames(proc_data) <- replace(x = colnames(proc_data), list = i, new_names[i])
}
colnames(proc_data) <- replace(x = colnames(proc_data), list = which(colnames(proc_data) == 'LDH'), 'HDL')

## 11- Add LDL variable
proc_data$LDL <- proc_data$Chol - proc_data$HDL - (proc_data$TG/5)
last_index <- length(colnames(proc_data))
proc_data <- subset(proc_data, select = c(1:33,last_index, 34:(last_index-1)))

## 12- use options values instead of options numbers in some tests and change names of cont vars
levels(proc_data$test229) <- c('uncontrolled', '3%', '1.5-2%', 'do not know', 'do not use')
levels(proc_data$test89) <- c('positive', 'negative')
levels(proc_data$test101) <- c('positive', 'negative')
levels(proc_data$test75) <- c('positive', 'negative')
levels(proc_data$test114) <- c('positive', 'negative')
levels(proc_data$test111) <- c('positive', 'negative')
levels(proc_data$test20) <- c('< 5hrs', '6-7hrs', '8-10hrs', '> 10hrs')
levels(proc_data$test232) <- c('positive', 'negative')
levels(proc_data$test32) <- c('1-2', '3-4', '5-6', '> 6', '0')
levels(proc_data$test58) <- c('too much', 'a lot', 'somewhat', 'low', 'not at all')
levels(proc_data$test22) <- c('0/week', '< 1/week', '1/week-2/week', '> 2/week')
levels(proc_data$test170) <- c('positive', 'negative')
levels(proc_data$test23) <- c('0/week', '< 1/week', '1/week-2/week', '> 2/week')
levels(proc_data$test54) <- c('very good', 'good', 'intermediate', 'bad', 'very bad')
levels(proc_data$test89) <- c('positive', 'negative')
levels(proc_data$test126) <- c('positive', 'negative')
levels(proc_data$test160) <- c('positive', 'negative')
levels(proc_data$test71) <- c('positive', 'negative')
# levels(proc_data$test154) <- c('positive', 'negative')
levels(proc_data$test102) <- c('positive', 'negative')
levels(proc_data$test2) <- c('male', 'female')
levels(proc_data$test62) <- c('positive', 'negative')
levels(proc_data$test1) <- c('20-29', '30-39', '40-49', '50-59', '60-69')

real_cont_names <- c('weight', 'height', 'body mass index (BMI)', 'metap', 'fat', 'systolic blood pressure', 'diastolic blood pressure', 'pulse', 'fat percentage', 'muscle percentage', 'waist circumference', 'hip circumference', 'neck circumference', 'fast serum glucose (FSG)', 'urinary albumin concentration (UAC)', 'cholesterol', 'triglyceride', 'creatinine', 'high-density lipoprotein (HDL)', 'white blood cell count (WBC)', 'red blood cell count (RBC)', 'hemoglobin', 'hematocrit', 'mean corpuscular volume (MCV)', 'mean cell hemoglobin (MCH)', 'mean corpuscular hemoglobin concentration (MCHC)', 'platelet', 'lymph', 'mixed cell count (MXD)', 'neutrophils', 'red blood cell distribution width (RDW)', 'platelet distribution width (PDW)', 'mean platelet volume (MPV)', 'low-density lipoproteins (LDL)')

cont_names <- colnames(proc_data[, 1:34])
names(cont_names) <- real_cont_names

