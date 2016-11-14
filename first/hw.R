# Прочетете данните и ги запишете в data frame в R;
filename = "pokemon.csv"

pokemonData = read.csv(filename, header=TRUE)

# Генерирайте си подизвадка от 600 наблюдения. 
# За целта нека f_nr е вашият факултетен номер. 
# Задайте състояние на генератора на 
# случайни числа в R чрез set.seed(f_nr). 
# С помощта на подходяща функция генерирайте 
# извадка без връщане на числата от 1 до 705 като
# не забравяте да я запишете във вектор. 
# Използвайте вектора, за да зашишете само 
# редовете със съответните индекси в нов дейтафрейм и
# работете с него оттук нататък;

f_nr = 71534
set.seed(f_nr)

indexes = sample(1:705, 600, replace=F)
subsample = subset(pokemonData, pokemonData$Number %in% indexes)

# Изкарайте на екрана първите няколко (5-6) наблюдения;
print("First six: ")
print(head(subsample, 6))

# Какъв вид данни (качествени/количествени, непрекъснати/дискретни)
# са записани във всяка от променливите?

# Number: качествен дискретен
# Name: качествен дискретен
# Type1: качествен непрекъснат
# Type2: качествен непрекъснат
# Attack: количествен дискретен
# Defense: количествен дискретен
# Height: количествен дискретен
# Weight: количествен дискретен

# Изведете дескриптивни статистики за всяка една от променливите;
print("Table of Type 1: ")
print(table(subsample$Type1))
print("Table of Type2: ")
print(table(subsample$Type2))
print("Fivenum of Attack: ")
print(fivenum(subsample$Attack))
print("Fivenum of Defense: ")
print(fivenum(subsample$Defense))
print("Fivenum of Height: ")
print(fivenum(subsample$Height))
print("Fivenum of Weight: ")
print(fivenum(subsample$Weight))

# Изведете редовете на най-високия и на най-лекия покемон;
# поради lazy evaluation ще сметне max и min само един път
print("Tallest pokemon: ")
print(subset(subsample, subsample$Height == max(subsample$Height)))

print("Slimest pokemon: ")
print(subset(subsample, subsample$Weight == min(subsample$Weight)))

# Изведете редовете на покемоните с общ брой точки за атака и защита
# над 220;
print("Best pokemons: ")
print(subset(subsample, subsample$Attack + subsample$Defense > 220))

# Колко на брой покемони имат първичен или вторичен тип "Dragon"или
# "Flying"и са високи над един метър?
types = c("Dragon", "Flying")
tallFlying = subset(subsample, subsample$Height > 1 & 
	(subsample$Type1 %in% types | subsample$Type2 %in% types))

print("Number of tall flying pokemons")
print(nrow(tallFlying))

# Направете хистограма на теглото само на 
# покемоните с вторичен тип и 
# нанесете графика на плътността върху нея. 
# Симетрично ли са разположени данните? - НЕ!!!

type2 = subset(subsample, !subsample$Type2  == "", select = c("Weight"))

vtype2 = (unlist(type2))
print(summary(vtype2))

hist(vtype2, 
	main="Histogram of Pokemon weight of pokemons with secondary type",
	xlab="Weight",
	probability=TRUE)
lines(density(vtype2))

boxplot(vtype2)

# За покемоните с първичен тип "Normal"или "Fighting"изследвайте
# съвместно променливите Type1 и Height с подходящ графичен метод.
# Забелязвате ли outlier-и? - Да
# Сравнете извадковите средни и медианите в
# двете групи и направете извод;

type1 = subset(subsample, 
	subsample$Type1  %in% c("Normal", "Fighting"), 
	select = c("Type1", "Height"))

print("Normal and Fighting")
print(type1)

 # If the notches of two plots do not overlap this is 
 # ‘strong evidence’ that the two medians differ
box = boxplot(type1$Height ~ type1$Type1, notch=TRUE)
print("Outliers: ")
print(box$out)

aggregate(type1$Height ~ type1$Type1, data=type1, FUN="mean")
aggregate(type1$Height ~ type1$Type1, data=type1, FUN="median")
# г/д Fighting са по-високи

# Изследвайте съвместно променливите Height и Weight с подходящ
# графичен метод. Бихте ли казали, че съществува линейна връзка меж-
# ду тях? Намерете корелацията между величините и коментирайте
# стойността ѝ. Начертайте регресионна права (линейната функция, ко-
# ято най-добре приближава функционалната зависимост). Ако е наб-
# людаван нов вид покемон с височина 2.1 метра, какво се очаква да е
# теглото му на базата на линейния модел?