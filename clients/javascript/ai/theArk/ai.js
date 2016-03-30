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
var lastShot;
var lastHit;
var movers = 0;
var shooting = false;





function randInt(min, max) {
  var range = max - min;
  var rand = Math.floor(Math.random() * (range + 1));
  return min + rand;
}


function radarCoords(config){
  var x = randInt((config.fieldRadius*-1)+3, config.fieldRadius-3);
  var y;
  if(x < 0){
    y = randInt((config.fieldRadius*-1)-x+3, config.fieldRadius-3);
  }
  else{
    y = randInt((config.fieldRadius*-1)+3, config.fieldRadius -x-3);
  }
  return {x:x, y:y};
}





function evaluate(events, bots, config){

  var retBotSet = [];
  var moveBots = [];
  var shootBots = {bool:false};


  //Check events
  for(var i = 0; i < events.length; i++){
    
    if(events[i].event === 'detected'  || events[i].event === 'damaged'){
      moveBots.push(events[i].botId);
    }

    if(events[i].event === 'see'){
      moveBots.push(events[i].source);
      shootBots = {bool:true, target:events[i].pos};
    }

    if(events[i].event === 'radarEcho'){
      shootBots = {bool:true, target:events[i].pos};
    }

    if(events[i].event === 'hit'){
      var t = true;
      for(var j = 0; j < bots.length; j++){
        if(bots[j].alive && events[i].botId == bots[j].botId){
          t = false;
        }
      }
      lastHit = t;
      
    }
  }


  //Assign bot directives

  for(var i = 0; i < bots.length; i++){
    if(bots[i].alive){
      
      var notMove = true;
      if(moveBots[0]){
        for(var j = 0; j < moveBots.length; j++){
          if(moveBots[j] == bots[i].botId){
            retBotSet.push({bot:bots[i], directive:"move"});
            notMove = false;
            movers++;
            continue;
          }
        }
      }
      if(!notMove)
        continue;


      if(shootBots.bool){
        retBotSet.push({bot:bots[i], directive:"cannon", target:shootBots.target});
      }

      if(!shootBots.bool){
        retBotSet.push({bot:bots[i], directive:"radar"});
      }

    }
  }

  

  return retBotSet;
}






module.exports = function Ai() {
  

  function makeDecisions(roundId, events, bots, config){
    
    //Evaluating the situation
    var doBots = evaluate(events, bots, config);  

    var shooters = doBots.length-movers;
    movers = 0;


    //Issuing commands
    for(var i = 0; i < doBots.length; i++){

      console.log(doBots[i].directive);

      if(doBots[i].directive === "move"){
        var set = randInt(0, moveSet.length-1);
        doBots[i].bot.move(moveSet[set][0]+doBots[i].bot.x, moveSet[set][1]+doBots[i].bot.y);
        continue;
      }

      if(doBots[i].directive === "cannon"){
        if(shooters === 1){
          lastShot = doBots[i];
          doBots[i].bot.cannon(doBots[i].target.x, doBots[i].target.y);
          continue;
        }

        if(shooters === 2){
          lastShot = doBots[i];
          doBots[i].bot.cannon(doBots[i].target.x, doBots[i].target.y);
          continue;
        }

        if(shooters === 3 && i < 2){
          lastShot = doBots[i];
          doBots[i].bot.cannon(doBots[i].target.x, doBots[i].target.y);
          continue;
        }
        else{
          doBots[i].bot.radar(doBots[i].target.x, doBots[i].target.y);
        }


      }

      if(doBots[i].directive === "radar"){
        if(!lastHit){
          var pos = radarCoords(config);
          doBots[i].bot.radar(pos.x, pos.y);
          continue;
        }
        else{
          lastShot.bot.cannon(lastShot.target.x, lastShot.target.y);
          lastHit = false;
          continue;
        }
      }

    }
    
  }
    


return {
    // The AI must return these three attributes
    botNames: botNames,
    makeDecisions: makeDecisions
  };
};