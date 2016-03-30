"use strict";

var _ = require("lodash");
var position = require("../../position.js");

// Change botNames and teamName to your choice.
var botNames = [
  "Captain",
  "Sgt.",
  "Private"
];



var moveSet = [[-2, 2], [-2, 1], [-2, 0], [-1, 2], [-1, -1], [0, 2], [0, -2], [1,1], [1, -2], [2, -2], [2, -1], [2, 0]];






function randInt(min, max) {
  var range = max - min;
  var rand = Math.floor(Math.random() * (range + 1));
  return min + rand;
}


/*function accurateGuess(roundID){
  var guessX = 0;
  var guessY = 0;
  var highX = 0;
  var lowX = 0;
  var highY = 0;
  var lowY = 0;
  for(i = 0; i < places.length; i++){
    guessX += places[i].x;
    guessY += places[i].y;
    if(highX < places[i].x)
      highX = places[i].x;
    if(highY < places[i].y)
      highY = places[i].y;
    if(lowX > places[i].x)
      lowX = places[i].x;
    if(lowY > places[i].y)
      lowY = places[i].y;
  } 
  guessX = guessX/places.length;
  guessY = guessY/places.length;
  guessX += (randomInteger(lowX, highX)*(1-(places.length/roundID)));
  guessY += (randomInteger(lowY, highY)*(1-(places.length/roundID)));
  guessX = Math.floor(guessX);
  guessY = Math.floor(guessY);
  if(guessX < 0)
    guessX = 0;
  if(guessX > MAP_SIZE-1)
    guessX = MAP_SIZE-1;
  if(guessY < 0)
    guessY = 0;
  if(guessY > MAP_SIZE-1)
    guessY = MAP_SIZE-1;
  return {x:guessX, y:guessY};
}*/





function evaluate(events, bots, config){

  var retBotSet = [];
  var moveBots = [];
  var shootBots = {bool:false};


  //Check events
  for(var i = 0; i < events.length; i++){
    
    if(events[i].event === 'detected'  || events[i].event === 'hit'){
      console.log("HERE");
      moveBots.push(events[i].botId);
      
    }

    if(events[i].event === 'radarEcho' || events[i].event === 'see'){
      shootBots = {bool:true, target:events[i].pos};
      console.log(events[i].pos);
    }
  }


  //Assign bot directives

  for(var i = 0; i < bots.length; i++){
    if(bots[i].alive){
      
      var notMove = true;

      if(moveBots.length < 0){
      for(var j = 0; j < moveBots.length; j++){
        if(moveBots[j] === bots[i].botId){
          retBotSet.push({bot:bots[i], directive:"move"});
          notMove = false;
        }
      }
    }

      if(notMove && shootBots.bool){
        retBotSet.push({bot:bots[i], directive:"cannon", target:shootBots.target});
      }

      if(notMove && !shootBots.bool){
        retBotSet.push({bot:bots[i], directive:"radar"});
      }

    }
  }

  return retBotSet;
}






module.exports = function Ai() {
  

  function makeDecisions(roundId, events, bots, config){
    
    //Evaluating the situation
    




    //Issuing commands
    for(var i = 0; i < bots.length; i++){

      
        var set = randInt(0, moveSet.length-1);
        bots[i].move(moveSet[set][0]+bots[i].x, moveSet[set][1]+bots[i].y);
      

     
    }
    
  }
    


return {
    // The AI must return these three attributes
    botNames: botNames,
    makeDecisions: makeDecisions
  };
};