# Dungate Stats
 
  # ALERT

  # Values generated, do not execute one more time 
  # Use pre-filled array!!!

# Values generation

##########################################
# gvRaw <- rbinom(50, 600, 0.14)
# gvRaw
##########################################

data = c(84,  79,  76,  77,  75,  87, 84,  90,  79,  86,  91,  84,  83,  80,
         81,  85,  82,  84,  81,  83,  78, 105,  81,  68,  88,  73,  76,  86,
         91,  86,  94,  77,  91,  87,  96,  85,  85,  80,  82,  76,  79,  74,
         78,  73,  85,  81,  93,  89,  77,  82)
data

#freq section
frequences = (table(data)/length(data))
sort(frequences, decreasing = FALSE)

#stima 10
subdata <- data[0:10]
stima10 = mean(subdata)/10
stima10

#stima 20
subdata <- data[0:20]
stima20 = mean(subdata)/20
stima20

#stima 30
subdata <- data[0:30]
stima30 = mean(subdata)/30
stima30

#stima 50
subdata <- data[0:50]
stima50 = mean(subdata)/50
stima50






