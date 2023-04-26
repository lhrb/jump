import Phaser from 'phaser'
import { io } from "socket.io-client"

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
        this.m = this.add.sprite(200, 400, 'monk', 'idle/idle_1.png');
        var idleFrames = this.anims.generateFrameNames('monk', {
            start: 1, end: 6, zeroPad:0,
            prefix: 'idle/idle_', suffix: '.png'
        });
        this.anims.create({ key: 'idle', frames: idleFrames, frameRate: 10, repeat: -1 });
        this.m.anims.play('idle');
    }

    update(time, delta) {
        this.m.x += delta/8;
        if (this.m.x > 800) {
            this.m.x = -50;
        }
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
