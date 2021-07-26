
#include <SoftwareSerial.h>
#define DEBUG true

// broche 10 de l'arduino en RX et broche 11 en TX : vous pouvez choisir d'autres broches
// connectez votre ESP8266 en mode croisé RX<=>TX
SoftwareSerial esp8266(10,11);

void setup()
{
  
  Serial.print("BEGIN");
  Serial.println(esp8266.available());
	Serial.begin(115200); // com serie de l' ARDUINO a 9600 bauds
	esp8266.begin(115200); // ESP8266 a 9600 bauds
  Serial.print(esp8266.available());
	sendData("AT+RST\r\n",2000,DEBUG); // reinitialise le module ESP8266
	sendData("AT+CWMODE=2\r\n",1000,DEBUG); // configure en TCP
	sendData("AT+CWSAP?\r\n",2000,DEBUG); // recupere nom et mot de passe de l' ESP
	sendData("AT+CIFSR\r\n",1000,DEBUG); // recupere l'adresse IP de l' ESP
	sendData("AT+CIPMUX=1\r\n",1000,DEBUG); // configure en connexion multiple
	sendData("AT+CIPSERVER=1,80\r\n",1000,DEBUG); // ouvre le serveur sur le port 80
  Serial.println("ENVOI MESSAGE");
}

void loop()
{
	if(esp8266.available()) // teste si l'ESP envoi un message
	{
  
    Serial.println("ENVOI MESSAGE");
		if(esp8266.find("+IPD,")) // avance jusqu'a trouver +IPD
		{
			delay(1000);
			int connectionId = esp8266.read()-48;
			// soustrait 48 parceque la fonction read() retourne la valeur ASCII decimale , zero (0) par exemple vaut 48 en ASCII decimal

			// ci dessous serie de commande qui renvoie du texte sur le port serie (page web)
			String webpage = "<h1>HELO</h1><h2>WORLD</h2><button>LED1</button>";
			String cipSend = "AT+CIPSEND=";
			cipSend += connectionId;
			cipSend += ",";
			cipSend +=webpage.length();
			cipSend +="\r\n";
			sendData(cipSend,1000,DEBUG);
			sendData(webpage,1000,DEBUG);
			webpage="<button>LED2</button>";
			cipSend = "AT+CIPSEND=";
			cipSend += connectionId;
			cipSend += ",";
			cipSend +=webpage.length();
			cipSend +="\r\n";
			sendData(cipSend,1000,DEBUG);
			sendData(webpage,1000,DEBUG);
			String closeCommand = "AT+CIPCLOSE=";
			closeCommand+=connectionId; // fermer la connexion
			closeCommand+="\r\n";
			sendData(closeCommand,3000,DEBUG);
		}
	}
}

// fonction sendData utilisee pour initialiser l' ESP
String sendData(String command, const int timeout, boolean debug)
{
	String response = "";
	esp8266.print(command); // envoi le caractere a l' esp8266
	long int time = millis();
	while( (time+timeout) > millis())
	{
		while(esp8266.available())
		{
			char c = esp8266.read(); // lit le caracter suivant
			response+=c;
		}
	}
	if(debug)
	{
		Serial.print(response);
	}
	return response;
}

 

 
