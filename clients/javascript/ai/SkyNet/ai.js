"use strict";

var _ = require("lodash");
var position = require("../../position.js");

// Change botNames and teamName to your choice.
var botNames = [
  "Captain",
  "Sgt.",
  "Private"
];


//wanted moves
var moveSet = [[-2, 2], [-2, 1], [-2, 0], [-1, 2], [-1, -1], [0, 2], [0, -2], [1,1], [1, -2], [2, -2], [2, -1], [2, 0]];


//var limMoveSet = [[-2, 0], [-1, -1], [0, -2], [0, 2], [1, 1], [2, 0]];
var lastShot;
var movers = 0;
var shooting = false;
var radarPoints = [];






function randInt(min, max) {
  var range = max - min;
  var rand = Math.floor(Math.random() * (range + 1));
  return min + rand;
}



//Function for OK radaring coordinates
function radarCoords(config, radarnum, radars){
  var coords = true;
  var x; 
  var y;
  while(coords){
    x = = randInt((config.fieldRadius*-1)+2, config.fieldRadius-2);
    if(x < 0){
      y = randInt((config.fieldRadius*-1)-x+2, config.fieldRadius-2);
    }
    else{
      y = randInt((config.fieldRadius*-1)+2, config.fieldRadius -x-2);
    }
    coords = false;
    if(radarPoints){
      for (int i = 0; i < radarPoints.length; i++){
        if(x>radarPoints[i][0]-6 && x<radarPoints[i][0]+6 && (( (x<radarPoints[i][0] && y> radarPoints[i][1]-6-(radarPoints[i][0]-x)) || (x<radarPoints[i][0] && y< radarPoints[i][1]+6 )) || ((x>radarPoints[i][0] && y< radarPoints[i][1]+6-(radarPoints[i][0]-x)) || (x<radarPoints[i][0] && y> radarPoints[i][1]-6 )))
          coords = true;
      }
    }

  }

  radarPoints.push([x, y]);
  if(radarnum === radars)
    radarPoints = [];


  return {x:x, y:y};
}




/*Returnset with each entry having the following:
    {bot: bot to command
    directive: what the bor should to (example "move")

    if a bot  needs to shoot, entry also has the following value

    target: {x, y}
  } */
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



    //Count how many of the bots are shooting
    var shooters = doBots.length-movers;
    movers = 0;

    //Keeping track of all the radaring bots
    var radarnum = 1;


    //Issuing commands
    for(var i = 0; i < doBots.length; i++){

      console.log(doBots[i].directive);

      if(doBots[i].directive === "move"){
        var set = -1;
        var move;

        //Choose a place on the map
        while(set<0){
          set = randInt(0, moveSet.length-1);
          move = {x:moveSet[set][0]+doBots[i].bot.x, y:moveSet[set][1]+doBots[i].bot.y}
          if(move.x<-14 || move.x>14 || (move.x<0 && move.y<((config.fieldRadius*-1)-move.x)) || (move.x<0 && move.y>config.fieldRadius) || (move.x>0 && move.y<(config.fieldRadius*-1)) || (move.x>0 && move.y>(config.fieldRadius -move.x)))
            set = -1;
        }


        doBots[i].bot.move(move.x, move.y);
        continue;
      }


      //Virhekoodia
      /*if(doBots[i].directive === "seen"){
        var set = -1;
        var move;
        while(set<0){
          set = randInt(0, limMoveSet.length-1);
          move = {x:limMoveSet[set][0]+doBots[i].bot.x, y:limMoveSet[set][1]+doBots[i].bot.y}
          if(move.x<-14 || move.x>14 || (move.x<0 && move.y<((config.fieldRadius*-1)-move.x)) || (move.x<0 && move.y>config.fieldRadius) || (move.x>0 && move.y<(config.fieldRadius*-1)) || (move.x>0 && move.y>(config.fieldRadius -move.x)))
            set = -1;
        }
        doBots[i].bot.move(move.x, move.y);
        continue;
      }*/



      //Following if-statement holds different objectives for all the bots based on how many are shooting.
      if(doBots[i].directive === "cannon"){
        if(shooters === 1){
          lastShot = doBots[i];
          doBots[i].bot.cannon(doBots[i].target.x, doBots[i].target.y);
          continue;
        }


        if(shooters === 2 && i == 0){
          lastShot = doBots[i];
          doBots[i].bot.cannon(doBots[i].target.x-1, doBots[i].target.y+1);
          continue;
        }
        if(shooters === 2 && i == 1){
          lastShot = doBots[i];
          doBots[i].bot.cannon(doBots[i].target.x+1, doBots[i].target.y-1);
          continue;
        }


        if(shooters === 3 && i == 0){
          lastShot = doBots[i];
          doBots[i].bot.cannon(doBots[i].target.x-1, doBots[i].target.y+1);
          continue;
        }
        if(shooters === 3 && i == 1){
          lastShot = doBots[i];
          doBots[i].bot.cannon(doBots[i].target.x+1, doBots[i].target.y-1);
          continue;
        }
        if(shooters === 3 && i == 2){
          doBots[i].bot.radar(doBots[i].target.x, doBots[i].target.y);
        }


      }

      
      if(doBots[i].directive === "radar"){
        if(!lastShot){
          var pos = radarCoords(config, radarnum, shooters);
          radarnum++;
          doBots[i].bot.radar(pos.x, pos.y);
          continue;
        }
        else{
          lastShot.bot.radar(lastShot.target.x, lastShot.target.y);
          lastShot = false;
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