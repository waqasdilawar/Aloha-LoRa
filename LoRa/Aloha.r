timeSpan = 60 * 1000;
#Mili Seconds
timeInterval = 10;
#Mili Seconds
numberOfSlots = timeSpan / timeInterval;
#For Randi Function
install.packages("pracma");
library(pracma);
frequencySpan = 125e3;
#Hertz :125 khz

frequencyInterval = 100;
# Hertez
startChannel = 1;
endChannel = 6;
numberOfChannels = endChannel;
fivePercent = 0;

maxNumberOfDevices = 100;
deviceStepSize = 1;

numberOfDevices = deviceStepSize:deviceStepSize:maxNumberOfDevices;
numberOfPackets = 1;
spreadFactor <- c(12L, 11L, 10L, 09L, 08L, 07L, 06L);
# For Passing to LoRa Matrix
bitRate1 <- c(293L, 547L, 976L, 1757L, 3125L, 5478L, 9375L);
#Bit Rate to check against Spreading Factor
bitRate2 <- c(682L, 365L, 204L, 113L, 64L, 36L, 21L);
#Bit Rate to check against Spreading Factor
loraDuration <- cbind(spreadFactor, bitRate1, bitRate2);
#row.names(loraDuration) <- c("SF With BitRate")
packetDuration = loraDuration[, 3];
#Packet Duration is according to BitRate2
results <- matrix(0, maxNumberOfDevices / deviceStepSize, 2);
for (noOfDevice in numberOfDevices) {
    #ft = zeros(nrofslots, nrofchannels);
    ft = matrix(0, numberOfSlots, numberOfChannels);
    #ft2 = zeros(nrofslots, nrofchannels);
    ft2 = matrix(0, numberOfSlots, numberOfChannels);
    #colission = zeros(nr, nrofpackets);
    colission = matrix(0, noOfDevice, numberOfPackets);
    sf = randi(c(startChannel, endChannel), noOfDevice, numberOfPackets);
    #time = randi([1 floor(nrofslots - (packetduration * numberOfPackets) / timeinterval)], [nr 1]);
    for (i in 1:noOfDevice) {
        timeOffset = floor((numberOfSlots - (packetDuration[sf[i] * numberOfPackets]) / timeInterval)
                           * rand(1, 1));
        for (p in 1:numberOfPackets) {
            for (k in 0:0) {
                if (sf[i,p] + k < 1 || (sf[i, p] + k > numberOfChannels))
                #continue in MATLAB is similar to Next in R
                    next;
                duration = loraDuration[sf[i, p], 3];
                flooredValue=floor(duration/timeInterval)
                for (j in 1:flooredValue) {
                    #freq(i, 1)
                    if (j + timeOffset > numberOfSlots) {
                        next;
                    }
                   currentIndex= ft[j + timeOffset, sf[i, p]];
                    if (!is.null(currentIndex) && currentIndex == 0) {
                        ft[j + timeOffset, sf[i, p] + k] = 1;
                        ft2[j + timeOffset, sf[i, p] + k] = i;
                    }
                    else {
                        ft[j + timeOffset, sf[i, p] + k] = ft[j + timeOffset, sf[i, p] + k] + 1;
                        colission[i] = 1;
                        colission[ft2[j + timeOffset, sf[i, p] + k]] = 1;
                    }
                }
            }
            timeOffset = ceil(timeOffset + duration / timeInterval);
        }
    }

    results[floor(noOfDevice / deviceStepSize), 1] = sum(colission);
    results[floor(noOfDevice / deviceStepSize), 2] = 100 * sum(colission) / noOfDevice;
    #Row wise sum
    fail = apply(colission, 1, sum) == numberOfPackets;

    if (results[floor(noOfDevice / deviceStepSize), 2] < 5) {
        fivePercent = noOfDevice;
    }
}
sortedResult <- sort(results[,1])
plot(numberOfDevices, results[, 1], main="Lora packet collision simulation withing 125 kH with \n %d devices
transmitting randomly within 60 seconds", xlab="Number of 25 byte  messages  minute", ylab="Nr of collisions or Fails",
type = "b", col = "red")

