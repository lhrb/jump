import Phaser from 'phaser'
import { Monk } from './monk.js'
import { Player } from './player.js';

let leftKey;
let rightKey;
let spaceKey;
let cursorsKeys;
let player;
let p;

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

function polygonPoints(x0, y0, r1, r2, offset) {
    const off = 72 * offset;
    // 72 degree split into 9
    const deg = [...Array(9).keys()]
        .map(i => i * 9 + off)
        .map(d => Phaser.Math.DegToRad(d));

    const inner = deg.map(d => {
            const c = Math.cos(d);
            const s = Math.sin(d);
            // inner
            const x = x0 + r1 * c;
            const y = y0 + r1 * s;
            return new Phaser.Geom.Point(x, y)
          });

    const outer = deg.map(d => {
        const c = Math.cos(d);
        const s = Math.sin(d);
        // outer
        const x = x0 + r2 * c;
        const y = y0 + r2 * s;
        return new Phaser.Geom.Point(x, y);
    }).reverse()

    return inner.concat(outer);
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
        let buttons = this.createButtons(750, 290);
        let bars = this.createBars();

        leftKey = this.input.keyboard.addKey('LEFT');
        rightKey = this.input.keyboard.addKey('RIGHT');
        spaceKey = this.input.keyboard.addKey('SPACE');

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
                         buttonA: buttons[0],
                         buttonB: buttons[1],
                         buttonC: buttons[2],
                         buttonD: buttons[3],
                         buttonE: buttons[4],
                         buttonF: buttons[5] },
                      bars);
    }

    update(time, delta) {
        p.update(time, delta);


        //JSON.stringify({action: "move", x: player.x});
        //ws.send(JSON.stringify({action: "move", x: player.x}));
    }

    createButtons(x, y) {
        const colors = [0xf94144, 0x577590, 0xf9c74f,
                        0xf9844a, 0x4d908e, 0x43aa8b];

        let buttons = [];
        // Create the middle button
        const middleButton = this.add.circle(x, y, 30, colors[0]);
        middleButton.setInteractive();
        const btnA = { gameObject: middleButton, isDown: false };
        middleButton.on('pointerdown', () => { btnA.isDown = true; })
                    .on('pointerup', () => { btnA.isDown = false; });
        buttons.push(btnA);

        for (let i = 0; i < 5; i++) {
            const points = polygonPoints(x, y, 31, 60, i);
            const btn = this.add.graphics();
            btn.fillStyle(colors[i + 1]);
            btn.fillPoints(points, true);
            btn.lineStyle(1, 0x000000, 1.0);
            btn.strokePoints(points, true);
            btn.setInteractive(new Phaser.Geom.Polygon(points), Phaser.Geom.Polygon.Contains);
            const btnB = { gameObject: btn, isDown: false };
            btn.on('pointerdown', () => { btnB.isDown = true; })
               .on('pointerup', () => { btnB.isDown = false; });

            buttons.push(btnB);
        }

        return buttons;
    }

    createBars() {
        const healthBarBg = this.add.graphics();
        healthBarBg.fillStyle(0x70271f, 1);
        healthBarBg.fillRect(10,10,200,10);

        const staminaBarBg = this.add.graphics();
        staminaBarBg.fillStyle(0x175631, 1);
        staminaBarBg.fillRect(10,21,200,10);

        const powerBarBg = this.add.graphics();
        powerBarBg.fillStyle(0x106066, 1);
        powerBarBg.fillRect(10,32,200,10);

        const healthBar = this.add.graphics();
        healthBar.fillStyle(0xe74c3c, 1);
        healthBar.fillRect(12,12,196,6);

        const setHealth = (percent) => {
            let width = Math.round(196 * percent);
            healthBar.clear();
            healthBar.fillStyle(0x15deed, 1);
            healthBar.fillRect(12,34,width,6);
        }

        const staminaBar = this.add.graphics();
        staminaBar.fillStyle(0x2ecc71, 1);
        staminaBar.fillRect(12,23,196,6);

        const setStamina = (percent) => {
            let width = Math.round(196 * percent);
            staminaBar.clear();
            staminaBar.fillStyle(0x15deed, 1);
            staminaBar.fillRect(12,34,width,6);
        }

        const powerBar = this.add.graphics();
        powerBar.fillStyle(0x15deed, 1);
        powerBar.fillRect(12,34,0,6);

        const setPower = (percent) => {
            let width = Math.round(196 * percent);
            powerBar.clear();
            powerBar.fillStyle(0x15deed, 1);
            powerBar.fillRect(12,34,width,6);
        }

        return { healthBar: setHealth,
                 staminaBar: setStamina,
                 powerBar: setPower };
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
