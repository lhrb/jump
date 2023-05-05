import Phaser from 'phaser'
import { Monk } from './monk.js'
import { Player } from './player.js';

let leftKey;
let rightKey;
let spaceKey;
let cursorsKeys;
let player;
let p;

let healthBar;
let staminaBar;

let buttonA = { gameObject: null, isDown: false };
let buttonB = { gameObject: null, isDown: false };
let buttonC = { gameObject: null, isDown: false };
let buttonD = { gameObject: null, isDown: false };

const ws = new WebSocket("ws://localhost:8080/websocket/");

let monk;

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

function playAnim(char, anim) {
    char.anims.play(monk.anim(anim), true)
        .once('animationcomplete', () => char.anims.play(monk.anim('idle')));
}

class Example extends Phaser.Scene
{

    constructor () {
        super();
    }

    preload () {
        this.load.multiatlas('monk', 'assets/monk.json', 'assets');

        // https://codepen.io/rexrainbow/pen/oyqvQY
        let url = 'https://raw.githubusercontent.com/rexrainbow/phaser3-rex-notes/master/dist/rexvirtualjoystickplugin.min.js';
        this.load.plugin('rexvirtualjoystickplugin', url, true);
    }

    create () {

        leftKey = this.input.keyboard.addKey('LEFT');
        rightKey = this.input.keyboard.addKey('RIGHT');
        spaceKey = this.input.keyboard.addKey('SPACE');

        healthBar = this.add.graphics();
        healthBar.fillStyle(0xe74c3c, 1);
        healthBar.fillRect(10,10,200,20);

        staminaBar = this.add.graphics();
        staminaBar.fillStyle(0x2ecc71, 1);
        staminaBar.fillRect(10,35,200,20);

        buttonA.gameObject = this.add.circle(780, 330, 15, 0x2ecc71).setInteractive();
        buttonB.gameObject = this.add.circle(755, 305, 15, 0x2ecc71).setInteractive();
        buttonC.gameObject = this.add.circle(780, 280, 15, 0x2ecc71).setInteractive();
        buttonD.gameObject = this.add.circle(805, 305, 15, 0x2ecc71).setInteractive();

        let textConfig = {fontSize:'20px', color:'white', fontFamily: 'Arial'};
        let txtA = this.add.text(0, 0, "A", textConfig);
        let txtB = this.add.text(0, 0, "B", textConfig);
        let txtC = this.add.text(0, 0, "C", textConfig);
        let txtD = this.add.text(0, 0, "D", textConfig);
        Phaser.Display.Align.In.Center( txtA, buttonA.gameObject );
        Phaser.Display.Align.In.Center( txtB, buttonB.gameObject );
        Phaser.Display.Align.In.Center( txtC, buttonC.gameObject );
        Phaser.Display.Align.In.Center( txtD, buttonD.gameObject );

        buttonA.gameObject
               .on('pointerdown', () => { buttonA.isDown = true; })
               .on('pointerup', () => { buttonA.isDown = false; });

         buttonB.gameObject
               .on('pointerdown', () => { buttonB.isDown = true; })
               .on('pointerup', () => { buttonB.isDown = false; });

         buttonC.gameObject
               .on('pointerdown', () => { buttonC.isDown = true; })
               .on('pointerup', () => { buttonC.isDown = false; });

         buttonD.gameObject
               .on('pointerdown', () => { buttonD.isDown = true; })
               .on('pointerup', () => { buttonD.isDown = false; });

        player = this.physics.add.sprite(200, 200, 'monk', 'idle/idle_1.png');

        player.setBounce(0.2);
        player.setCollideWorldBounds(true);

        monk = new Monk(this).create();

        player.anims.play(monk.anim('run'))
              .once('animationcomplete', () => player.anims.play(monk.anim('idle')));

        player.on('animationupdate', (animation,frame,gameObject,frameKey) => {
            player.body.setSize(frame.frame.width, frame.frame.height)
        });

        this.joyStick = this.plugins.get('rexvirtualjoystickplugin').add(this, {
                x: 60,
                y: 300,
                radius: 50,
                base: this.add.circle(0, 0, 50, 0x888888),
                thumb: this.add.circle(0, 0, 25, 0xcccccc),
            });

        cursorsKeys = this.joyStick.createCursorKeys();
        p = new Player(player,
                       monk,
                       { cursorsKeys: cursorsKeys,
                         buttonA: buttonA,
                         buttonB: buttonB,
                         buttonC: buttonC,
                         buttonD: buttonD });
    }

    update(time, delta) {
        p.update(time, delta);


        //JSON.stringify({action: "move", x: player.x});
        //ws.send(JSON.stringify({action: "move", x: player.x}));
    }
}

const config = {
    type: Phaser.AUTO,
    physics: {
        default: 'arcade',
        arcade: {
            gravity: { y: 200 },
            debug: true
        }
    },
    scale: {
        mode: Phaser.Scale.FIT,
        autoCenter: Phaser.Scale.CENTER_BOTH,
        width: 840,
        height: 360,
    },
    input :{
        activePointers:3,
    },
    scene: Example
};

new Phaser.Game(config)
