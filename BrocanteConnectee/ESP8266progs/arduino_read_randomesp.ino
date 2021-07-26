//Try to connect The GPIO0 with a 10k resistor to ground and the CH_PD to VCC and it works :) 3.9k
#include <SoftwareSerial.h>
#include <LiquidCrystal.h> //Header file for LCD from https://www.arduino.cc/en/Reference/LiquidCrystal


const int E1 = 3; ///<Motor1 Speed
const int E2 = 11;///<Motor2 Speed
const int M1 = 4; ///<Motor1 Direction
const int M2 = 12;///<Motor2 Direction
String payload;

SoftwareSerial ESP_Serial(6, 7); //Tx,Rx


///<Motor1 Advance
void M1_advance(char Speed){
 digitalWrite(M1,LOW);
 analogWrite(E1,Speed);
}

///<Motor2 Advance
void M2_advance(char Speed){
 digitalWrite(M2,HIGH);
 analogWrite(E2,Speed);
}

///<Motor1 Back off
void M1_back(char Speed){
 digitalWrite(M1,HIGH);
 analogWrite(E1,Speed);
}

///<Motor2 Back off
void M2_back(char Speed) 
{
 digitalWrite(M2,LOW);
 analogWrite(E2,Speed);
}


void setup() {
  Serial.begin(115200);

  pinMode( 3,OUTPUT);
  pinMode( 4,OUTPUT);
  pinMode( 11,OUTPUT);
  pinMode( 12,OUTPUT);
  delay(2000);
}

void loop() {
  while (ESP_Serial.available() > 0)
  {
    payload = ESP_Serial.readString();


    delay(10);
    Serial.println(payload);
    

  }
}