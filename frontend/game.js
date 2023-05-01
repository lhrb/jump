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

function playAnim(char, anim) {
    char.anims.play(anim, true)
        .once('animationcomplete', () => char.anims.play('idle'));
}

class Example extends Phaser.Scene
{

    constructor () {
        super();
    }

    preload () {
        this.load.multiatlas('monk', 'assets/monk.json', 'assets');
    }

    create () {

        leftKey = this.input.keyboard.addKey('LEFT');
        rightKey = this.input.keyboard.addKey('RIGHT');

        player = this.physics.add.sprite(200, 200, 'monk', 'idle/idle_1.png');
        player.setScale(2,2);

        player.setBounce(0.2);
        player.setCollideWorldBounds(true);

        let idleFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 6, zeroPad:0,
            prefix: 'idle/idle_', suffix: '.png'
        });
        this.anims.create({ key: 'idle', frames: idleFrames, frameRate: 10, repeat: -1 });

        let runFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 8, zeroPad:0,
            prefix: 'run/run_', suffix: '.png'
        });
        this.anims.create({ key: 'run', frames: runFrames, frameRate: 10, repeat: 0 });

        let jumpUpFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 3, zeroPad:0,
            prefix: 'j_up/j_up_', suffix: '.png'
        });
        this.anims.create({ key: 'jumpUp', frames: jumpUpFrames, frameRate: 10, repeat: 0 });

        let jumpDownFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 3, zeroPad:0,
            prefix: 'j_down/j_down_', suffix: '.png'
        });
        this.anims.create({ key: 'jumpDown', frames: jumpDownFrames, frameRate: 10, repeat: 0 });

        let airAttackFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 7, zeroPad:0,
            prefix: 'air_atk/air_atk_', suffix: '.png'
        });
        this.anims.create({ key: 'airAttack', frames: airAttackFrames, frameRate: 10, repeat: 0 });

        let attack1Frames = this.anims.generateFrameNames('monk', {
            start: 1, end: 6, zeroPad:0,
            prefix: '1_atk/1_atk_', suffix: '.png'
        });
        this.anims.create({ key: 'attack1', frames: attack1Frames, frameRate: 10, repeat: 0 });

        let attack2Frames = this.anims.generateFrameNames('monk', {
            start: 1, end: 12, zeroPad:0,
            prefix: '2_atk/2_atk_', suffix: '.png'
        });
        this.anims.create({ key: 'attack2', frames: attack2Frames, frameRate: 10, repeat: 0 });

        let attack3Frames = this.anims.generateFrameNames('monk', {
            start: 1, end: 23, zeroPad:0,
            prefix: '3_atk/3_atk_', suffix: '.png'
        });
        this.anims.create({ key: 'attack3', frames: attack3Frames, frameRate: 10, repeat: 0 });

        let specialAttackFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 25, zeroPad:0,
            prefix: 'sp_atk/sp_atk_', suffix: '.png'
        });
        this.anims.create({ key: 'specialAttack', frames: specialAttackFrames, frameRate: 10, repeat: 0 });

        let meditateFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 16, zeroPad:0,
            prefix: 'meditate/meditate_', suffix: '.png'
        });
        this.anims.create({ key: 'meditate', frames: meditateFrames, frameRate: 10, repeat: 0 });

        let rollFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 6, zeroPad:0,
            prefix: 'roll/roll_', suffix: '.png'
        });
        this.anims.create({ key: 'roll', frames: rollFrames, frameRate: 10, repeat: 0 });

        let defendFrames  = this.anims.generateFrameNames('monk', {
            start: 1, end: 13, zeroPad:0,
            prefix: 'defend/defend_', suffix: '.png'
        });
        this.anims.create({ key: 'defend', frames: defendFrames, frameRate: 10, repeat: 0 });

        let takeHitFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 6, zeroPad:0,
            prefix: 'take_hit/take_hit_', suffix: '.png'
        });
        this.anims.create({ key: 'takeHit', frames: takeHitFrames, frameRate: 10, repeat: 0 });

        let deathFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 18, zeroPad:0,
            prefix: 'death/death_', suffix: '.png'
        });
        this.anims.create({ key: 'death', frames: deathFrames, frameRate: 10, repeat: 0 });

        player.anims.play('run')
              .once('animationcomplete', () => player.anims.play('idle'));
    }

    update(time, delta) {

        if (leftKey.isDown) {
            //player.scaleX = -2;
            player.setVelocityX(105);
            playAnim(player, 'run');
        } else if (rightKey.isDown) {
            //player.scaleX = 2;
            //player.x += delta/8;
            player.setVelocityX(-5);
            playAnim(player, 'run');
        }

        /*this.m.x += delta/8;
        if (this.m.x > 800) {
            this.m.x = -50;
        }*/

        //ws.send(JSON.stringify({action: "move", x: player.x}));
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
