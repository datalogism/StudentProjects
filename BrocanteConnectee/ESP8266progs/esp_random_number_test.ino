//Try to connect The GPIO0 with a 10k resistor to ground and the CH_PD to VCC and it works :) 3.9k
#include <SoftwareSerial.h>

String payload;

SoftwareSerial ESP_Serial(6, 7); //Tx,Rx



void setup() {
  Serial.begin(9600);
  ESP_Serial.begin(115200);
  delay(2000);
}

void loop() {
  while (ESP_Serial.available() > 0)
  {

    Serial.println("INSIDE : ");
    payload = ESP_Serial.readString();


    delay(10);
    Serial.println(payload);
    

  }

    Serial.println("OUT..");
}