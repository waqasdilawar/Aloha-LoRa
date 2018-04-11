sourceNumber <- 100;
packetReadyProb <- 0.0057;
maxBackoff <- 100;
simulationTime <- 5000;
#For Randi Function
install.packages("pracma");
library(pracma);
sourceStatus <- matrix(0, 1, sourceNumber);

#% legit source statuses are always non - negative integers and equal to:
#% 0:source has no packet ready to be transmitted(is idle)
#% 1:source has a packet ready to be transmitted, either because new data must be sent or a previously collided packet has waited the backoff time
#% > 1:source is backlogged due to previous packets collision, the value of the status equals the number of slots it must wait for
#the next transmission attempt
sourceBackoff <- matrix(0, 1, sourceNumber);

packtTransmissionAttempts = 0;
ackdPacketDelay <- matrix(0, 1, simulationTime);

ackdPacketCount = 0;
packtCollisionCount = 0;
packtGenerationTimestamp <- matrix(0, 1, sourceNumber);
currentSlot = 0;
while (currentSlot < simulationTime) {
    currentSlot = currentSlot + 1;
    for (eachSource1 in 1:length(sourceStatus)) {
        #% new packet
        if (sourceStatus[1, eachSource1] == 0 && rand(1) <= packetReadyProb) {
            sourceStatus[1, eachSource1] = 1;
            sourceBackoff[1, eachSource1] = randi(maxBackoff, 1);
            packtGenerationTimestamp[1, eachSource1] = currentSlot;
        }
        else if (sourceStatus[1, eachSource1] == 1) {
            #% backlogged packet
            sourceBackoff[1, eachSource1] = randi(maxBackoff, 1);
        }
        packtTransmissionAttempts = packtTransmissionAttempts + sum(sourceStatus == 1);
    }

    if (sum(sourceStatus == 1) == 1) {
        ackdPacketCount = ackdPacketCount + 1;
        sourceId <- which(sourceStatus == 1, arr.ind = T)
        ackdPacketDelay[ackdPacketCount] = currentSlot - packtGenerationTimestamp[sourceId];
    } else if (sum(sourceStatus == 1) > 1) {
        packtCollisionCount = packtCollisionCount + 1;
        sourceStatus = sourceStatus + sourceBackoff;
    }
    sourceStatus = sourceStatus - 1;
    #% decrease backoff interval
    sourceStatus[sourceStatus < 0] = 0;
    #% idle sources stay idle(see permitted statuses above)
    sourceBackoff <- matrix(0, 1, sourceNumber);
}
trafficOffered = packtTransmissionAttempts / currentSlot;
if (ackdPacketCount == 0) {
    meanDelay = simulationTime;
    #% theoretically, if packets collide continously, the delay tends to infinity
} else {
    meanDelay = mean(ackdPacketDelay[1,ackdPacketCount]);
}
throughput = ackdPacketCount / currentSlot;
packtCollisionProb = packtCollisionCount / currentSlot;

throughput <- throughput * 100;

print(paste("ThroughPut is =", throughput,"%"));
