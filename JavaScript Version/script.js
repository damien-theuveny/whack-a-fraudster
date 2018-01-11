// on load setup
$(document).ready(function() {
    $('body').css("display","none");
    var x = document.cookie;
    console.log(x);
    if(x.search("damienGame=auth") != -1){
        $('body').css("display","block");
        start();
        setInstructions();
    }
    else{
        // display password box
        $('html').append("<input class='p-input' type='text' name='password'><div class='submit button'>Submit</div>");
        $(".submit").click(function(){
            var password = $('.p-input').val();
            console.log(password);
            if(password == "dog"){
                document.cookie = "damienGame=auth";
                $('body').css("display","block");
                $('.p-input').css("display","none");
                $('.submit').css("display","none");
                start();
                setInstructions();
            }
        });
    }
});

// on resize changes
$(window).resize(function() {
    $('body').css("height",window.innerHeight + "px");
    setInstructions();
});

/* ---- Global vars ----- */
// change to array
var score = [0,0,0];
var playing = false;
var difficulty = 2;
var superBadGuy = setSuperBadGuy();
var superBadGuyPassed = true;
var level = 1;

function start(){
    $('body').css("height",window.innerHeight + "px");
        $(".startButton").click(function(){
            if($('.startButton').html() == "Start Game"){
                startGame();
            }
            else{
                showFinalScore();
            }
        });
        $(".close").click(function(){
            resetGame();
        });
    setInstructions();
}

function setInstructions() {
    var col_width = $('.instructions .col').innerWidth();
    var numberInRow = 2;
    var leftOverSpace = col_width - numberInRow*50;
    if(leftOverSpace != 0){
        var margin = Math.floor((Math.floor(leftOverSpace/numberInRow))/2);
        $('.col .icon').css("margin","20px "+ (margin-12) + "px 20px "+ (margin-12) + "px");
    }
    else{
        $('.col .icon').css("margin","20px 10px");
    }
}


function animateLargeChars() {
    // animate chars
    TweenLite.killDelayedCallsTo(hideChars);
    $('.leftChars').css("display","block");
    $('.rightChars').css("display","block");
    TweenLite.to($('.largeSuperBadGuy'), 1, {left:"-10%"});
    TweenLite.to($('.largeBadGuy'), 1, {delay:0.5,left:"-20%"});
    
    TweenLite.to($('.customer1'), 1, {right:"-40%"});
    TweenLite.to($('.customer2'), 1, {delay:0.5,right:"-65%"});
}

function animateLargeCharsOut(){
    TweenLite.to($('.largeSuperBadGuy'), 1, {left:"-100%"});
    TweenLite.to($('.largeBadGuy'), 1, {delay:0.5,left:"-100%"});
    
    TweenLite.to($('.customer1'), 1, {right:"-100%"});
    TweenLite.to($('.customer2'), 1, {delay:0.5,right:"-100%"});
    TweenLite.delayedCall(1.5, hideChars);
}

function hideChars(){
    $('.leftChars').css("display","none");
    $('.rightChars').css("display","none");
}


// reset && setup game
function resetGame() {
    playing = false;
    animateLargeCharsOut();
    TweenLite.killDelayedCallsTo(removeBadGuy);
    TweenLite.killDelayedCallsTo(failed);
    TweenLite.killDelayedCallsTo(removeCustomer);
    TweenLite.killDelayedCallsTo(showFinalScore);
    TweenLite.killDelayedCallsTo(generateSuperBadGuy);
    $('.score').css("display","none");
    $('.finalScoreContainer').css("display","none");
    $('.instructions').css("display","block");
    $('.startButton').html("Start Game");
    $('.startButton').css("display","block");
    $('.close').css("display","none");
    $('.gridContainer').css("display","none");
    setInstructions();
}

function startGame() {
    playing = true;
    score = [0,0,0];
    difficulty = 2;
    $('.score').html("");
    $('.score').css("display","block");
    $('.instructions').css("display","none");
    $('.startButton').html("End Game");
    $('.close').css("display","block");
    setGrid(3);
    superBadGuy = setSuperBadGuy();
    superBadGuyPassed = true;
    level = 1;
    $('.gridContainer').css("display","block");
    checkSuperBadGuy();
    animateLargeChars();
    TweenLite.delayedCall(45, showFinalScore);
}

// set grid
function setGrid(size) {
    
    // initialise grid with number of elements
    $('.gridContainer').html("");
    for(var i=0;i < size*size; i++){
        $('.gridContainer').append("<div class='gridBox'></div>");
    }
    
    // size and position grid correctly
    $('.gridContainer').css("width",size*50 + "px");
    $('.gridContainer').css("top","calc(50% - " + (size*50)/2 + "px)");
    $('.gridContainer').css("left","calc(50% - " + (size*50)/2 + "px)");
    
    //set centre element to be logo
    var logoChild = Math.floor($('.gridContainer').children().length/2)+1;
    $( ".gridBox:nth-of-type("+ logoChild +")").addClass("gridLogo");
    
    
    $(".gridBox").click(function(){
        if($(this).hasClass("badGuy")){
            TweenLite.killDelayedCallsTo(failed);
            updateScore("badGuy");
            $(this).removeClass("badGuy");
        }
        else if($(this).hasClass("customer")){
            updateScore("customer");
            $(this).removeClass("customer");
        }
        else if($(this).hasClass("superBadGuy")){
            superBadGuyPassed = true;
            TweenLite.killDelayedCallsTo(failed);
            updateScore("superBadGuy");
            $(this).removeClass("superBadGuy");
        }
    });
    generateBadGuy();
    generateCustomer();
    generateCustomer();
}

function setSuperBadGuy(){
    return whichLevel = Math.floor(Math.random(3)*3)+1;
}

function checkSuperBadGuy(){
    if(level == superBadGuy){
        superBadGuyPassed = false;
        var randomSuperBadGuyDelay = Math.floor(Math.random(8)*8)+1;
        TweenLite.delayedCall(randomSuperBadGuyDelay, generateSuperBadGuy);
        console.log(randomSuperBadGuyDelay + " seconds until super bad guy appears");
    }
}

//make customer // customer / bad guy generating can be condensed into one function with a parameter to distinguish
function generateCustomer(){
    tile = randomTile();
    $(".gridBox:nth-of-type("+ tile +")").removeClass("image-two");
    $(".gridBox:nth-of-type("+ tile +")").addClass("customer");
    if(Math.floor(Math.random(1)*2) == 1){
        $(".gridBox:nth-of-type("+ tile +")").addClass("image-two");
    }
    TweenLite.delayedCall(difficulty, removeCustomer, [tile]); 
    
}

//make badGuy // customer / bad guy generating can be condensed into one function with a parameter to distinguish
function generateBadGuy(){
    tile = randomTile();
    $(".gridBox:nth-of-type("+ tile +")").removeClass("image-two");
    $(".gridBox:nth-of-type("+ tile +")").addClass("badGuy");
    if(Math.floor(Math.random(1)*2) == 1){
        $(".gridBox:nth-of-type("+ tile +")").addClass("image-two");
    }
    TweenLite.delayedCall(difficulty, removeBadGuy, [tile]);
}

function generateSuperBadGuy(){
    tile = randomTile();
    $(".gridBox:nth-of-type("+ tile +")").removeClass("image-two");
    $(".gridBox:nth-of-type("+ tile +")").addClass("superBadGuy");
    TweenLite.delayedCall(3, removeSuperBadGuy, [tile]);
}


function randomTile(){
    var gridLength = $('.gridContainer').children().length;
    // check if the tile is the logo tile
    var tile = Math.floor($('.gridContainer').children().length/2)+1;
    while(checkTileOccupied(tile)){
        tile = Math.floor(Math.random(gridLength)*gridLength+1);
    }
    return tile;
}

function checkTileOccupied(tile){
    if($(".gridBox:nth-of-type("+ tile +")").hasClass("badGuy")||$(".gridBox:nth-of-type("+ tile +")").hasClass("customer")||$(".gridBox:nth-of-type("+ tile +")").hasClass("gridLogo")||$(".gridBox:nth-of-type("+ tile +")").hasClass("superBadGuy")){
        return true;
    }
    else{
        return false;
    }
}


function removeBadGuy(tile){
    $(".gridBox:nth-of-type("+ tile +")").removeClass("badGuy");
    generateBadGuy();
}

function removeCustomer(tile){
    $(".gridBox:nth-of-type("+ tile +")").removeClass("customer");
    generateCustomer();
}

function removeSuperBadGuy(tile){
    $(".gridBox:nth-of-type("+ tile +")").removeClass("superBadGuy");
    superBadGuyPassed = true;
}

// level failed
function failed() {
    showFinalScore();
}

function getCurrentGridSize(){
    return Math.sqrt($('.gridContainer').children().length);
}

// level passed
function passed() {
    // added 500 points on current size or 10 seconds passed
    console.log("passed");
    setGrid(getCurrentGridSize() + 2);
    difficulty = difficulty - 0.25;
    level = level + 1;
    checkSuperBadGuy();
}

// update score // change to take two paramas (bad guy clicks & customer clicks) and calculate the score accordingly for end game statistics
function updateScore(type) {
    if(type == "badGuy"){
        score[0] = score[0] + 1;
    }
    else if(type == "customer"){
        score[1] = score[1] + 1;
    }
    else if(type == "superBadGuy"){
        score[2] = score[2] + 1;
    }
    $('.score').html("Score: " + ((score[0]*50) + (score[1]*-100) + (score[2]*250)));
    if(((score[0]*50) + (score[1]*-100) + (score[2]*250)) >= 350 && getCurrentGridSize() == 3 && superBadGuyPassed){
        passed();
    }
    else if(((score[0]*50) + (score[1]*-100) + (score[2]*250)) >= 800 && getCurrentGridSize() == 5 && superBadGuyPassed){
        passed();
    }
    else if(((score[0]*50) + (score[1]*-100) + (score[2]*250)) >= 1600 && getCurrentGridSize() == 7 && superBadGuyPassed){
        passed();
    }
    TweenLite.delayedCall(5, failed);
}

//
function showFinalScore() {
    // end game
    animateLargeCharsOut();
    $('.startButton').css("display","none");
    $('.gridContainer').css("display","none");
    $('.finalScoreContainer').css("display","block");
    var superbad = "";
    if(score[2] > 0){
        superbad = "you caught the super bad fraudster";
    }
    else{
        superbad = "you didn't catch the super bad fraudster"
    }
    $('.finalScoreHeader').html("You denied " + score[0] + " fraudsters, " + score[1] + " genuine customers and " + superbad);
    generateChart();
}

function generateChart(){
    var chartWidth = $('.chart').width();
    var lineWidth = chartWidth/2;
    var totalScore = ((score[0]*50) + (score[1]*-100) + (score[2]*250));
    var pointEqualsPixel = Math.abs(lineWidth/totalScore);
    $('.badGuyBlock').css("width", pointEqualsPixel * (score[0]*50) + "px");
    $('.customerBlock').css("width", pointEqualsPixel * (score[1]*100) + "px");
    console.log(pointEqualsPixel * (score[1]*100));
    $('.superBadGuyBlock').css("width", pointEqualsPixel * (score[2]*250) + "px");
}


// submit score to leaderboard
function submitScore() {
    // use indexedDB to store the score on the device
}



 /* indexed Database
 
 
 
//prefixes of implementation that we want to test
window.indexedDB = window.indexedDB || window.mozIndexedDB || window.webkitIndexedDB || window.msIndexedDB;
 
//prefixes of window.IDB objects
window.IDBTransaction = window.IDBTransaction || window.webkitIDBTransaction || window.msIDBTransaction;
window.IDBKeyRange = window.IDBKeyRange || window.webkitIDBKeyRange || window.msIDBKeyRange
 
if (!window.indexedDB) {
    window.alert("Your browser doesn't support a stable version of IndexedDB.")
}

var db;
var request = window.indexedDB.open("newDatabase", 4);
 
request.onerror = function(event) {
  console.log("error: ");
};
 
request.onsuccess = function(event) {
  db = request.result;
  console.log("success: "+ db);
};
 
request.onupgradeneeded = function(event) {
        var db = event.target.result;
        var objectStore = db.createObjectStore("players", {keyPath: "id"});
}
 
function readAll() {
        var objectStore = db.transaction("players").objectStore("players");
  
        objectStore.openCursor().onsuccess = function(event) {
          var cursor = event.target.result;
          if (cursor) {
                alert("Name for id " + cursor.key + " is " + cursor.value.name + ", Age: " + cursor.value.age + ", Email: " + cursor.value.email);
                cursor.continue();
          }
          else {
                alert("No more entries!");
          }
        };     
}
 
function add() {
        var request = db.transaction(["players"], "readwrite")
                .objectStore("players")
                .add({ id: "00-03", name: "Kenny", age: 19, email: "kenny@planet.org" });
                                 
        request.onsuccess = function(event) {
                alert("Kenny has been added to your database.");
        };
         
        request.onerror = function(event) {
                alert("Unable to add data\r\nKenny is aready exist in your database! ");       
        }
         
}

*/
 