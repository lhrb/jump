import Phaser from 'phaser'

let leftKey;
let rightKey;
let player;

const ws = new WebSocket("ws://localhost:8080/websocket/");

// Connection opened
ws.addEventListener("open", (event) => {
  ws.send("Hello Server!");
});

// Listen for messages
ws.addEventListener("message", (event) => {
    console.log("Message from server ", event.data);
    const action = JSON.parse(event.data);
    switch (action.action) {
        case "move":
            player.x = action.x;
            break;
    }
});



class Example extends Phaser.Scene
{

    constructor () {
        super();
        this.m = null;
    }

    preload () {
        this.load.multiatlas('monk', 'assets/monk.json', 'assets');
    }

    create () {

        leftKey = this.input.keyboard.addKey('LEFT');
        rightKey = this.input.keyboard.addKey('RIGHT');


        player = this.add.sprite(200, 200, 'monk', 'idle/idle_1.png');
        player.setScale(4,4);
        var idleFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 6, zeroPad:0,
            prefix: 'idle/idle_', suffix: '.png'
        });
        this.anims.create({ key: 'idle', frames: idleFrames, frameRate: 10, repeat: -1 });
        player.anims.play('idle');
    }

    update(time, delta) {

        if (leftKey.isDown) {
            player.x -= delta/8;
        } else if (rightKey.isDown) {
            player.x += delta/8;
        }

        /*this.m.x += delta/8;
        if (this.m.x > 800) {
            this.m.x = -50;
        }*/

        ws.send(JSON.stringify({action: "move", x: player.x}));
    }
}

const config = {
    type: Phaser.AUTO,
    width: 800,
    height: 600,
    physics: {
        default: 'arcade',
        arcade: {
            gravity: { y: 200 }
        }
    },
    scene: Example
};

new Phaser.Game(config)
