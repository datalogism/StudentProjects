#include <SoftwareSerial.h>
#define DEBUG true

SoftwareSerial esp8266(10,11); // RX Arduino sur broche 2 , TX arduino sur broche 3.
void setup()
{
 Serial.print("BEGIN");
  Serial.println(esp8266.available());
  Serial.begin(115200); // arduino a 115200 bauds
  esp8266.begin(115200); // esp a 115200 bauds
  pinMode(7,OUTPUT); // config des broches 11 12 et 13
  digitalWrite(7,LOW);
  pinMode(6,OUTPUT);
  digitalWrite(6,LOW);
  pinMode(5,OUTPUT);
  digitalWrite(5,LOW);
  sendData("AT+RST\r\n",2000,DEBUG); // reset du module
  sendData("AT+CWMODE=2\r\n",1000,DEBUG); // module en point d'acces
  sendData("AT+CIFSR\r\n",1000,DEBUG); // recupere l'adresse IP
  sendData("AT+CIPMUX=1\r\n",1000,DEBUG); // config en connexion multiple
  sendData("AT+CIPSERVER=1,80\r\n",1000,DEBUG); // serveur sur port 80
 
  Serial.println("END SETUP");
  Serial.println(esp8266.available());
}

void loop()
{
  
  if(esp8266.available()) // verifie si esp envoi un message
  {
  
      Serial.println("HEY");
    if(esp8266.find("+IPD,"))
    {
    
      delay(1000); // delai pour remplissage du tampon de données
     
      Serial.println("HEY");
      // recup de l' ID de connection
      int connectionId = esp8266.read()-48;
      esp8266.find("pin=");
      int pinNumber = esp8266.read()-48;
      
       
      Serial.println(">>>>>>> pinNumber : ");
      Serial.println(pinNumber);
      
      digitalWrite(pinNumber, !digitalRead(pinNumber));
      // bloc de commande de fermeture de connexion
      String closeCommand = "AT+CIPCLOSE=";
      closeCommand+=connectionId;
      closeCommand+="\r\n";
     
      sendData(closeCommand,1000,DEBUG);
    }
  }
}

/*
* fonction : sendData = envoi des données a l' ESP
*/
String sendData(String command, const int timeout, boolean debug)
{
  
  Serial.println("HEY");
  String response = "";
  esp8266.print(command);
  long int time = millis();
  while( (time+timeout) > millis())
  {
    while(esp8266.available())
    {
      char c = esp8266.read();
      response+=c;
    }
  }

  if(debug)
  {
    Serial.print(response);
  }

  return response;
}
