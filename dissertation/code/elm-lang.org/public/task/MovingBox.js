/* 

Try moving the square around with your keyboard's arrow keys

Click your mouse over there =====> 
Use arrows Up, Down, Left, Right

Whee!

Now modify the code to prevent the square from going outside 
the edge of the grey window.

*/

var WIDTH = 400;
var HEIGHT = 400;
var SQUARE = 40;
var COLORS = [
    "0x000000",
    "0xCCCCCC",
];
var MOVEMENT_SPEED = 5;

var stage = new PIXI.Stage(COLORS[1]);
var renderer = PIXI.autoDetectRenderer(WIDTH, HEIGHT);
document.body.appendChild(renderer.view);

var box = new PIXI.Graphics();
box.lineStyle(1, COLORS[0], 1);
box.beginFill(COLORS[1], 0);
box.drawRect(0, 0, SQUARE, SQUARE);
box.endFill();
stage.addChild(box);

box.x = (WIDTH / 2) - (SQUARE / 2);
box.y = (HEIGHT / 2) - (SQUARE / 2);

var keyState = {};

window.addEventListener('keydown', function(e) {
    keyState[e.keyCode || e.which] = true;
}, true);

window.addEventListener('keyup', function(e) {
    keyState[e.keyCode || e.which] = false;
}, true);

requestAnimFrame(animate);

function animate() {
    if (keyState[37]) {
        box.x -= MOVEMENT_SPEED;
    }

    if (keyState[38]) {
        box.y -= MOVEMENT_SPEED;
    }

    if (keyState[39]) {
        box.x += MOVEMENT_SPEED;
    }

    if (keyState[40]) {
        box.y += MOVEMENT_SPEED;
    }

    renderer.render(stage);
    requestAnimFrame(animate);
}
