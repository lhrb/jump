export class Monk {

    #anims =
        {
            'idle': 'idleMonk',
            'run': 'runMonk',
            'jumpUp': 'jumpUpMonk',
            'jumpDown': 'jumpDownMonk',
            'airAttack': 'airAttackMonk',
            'attack1': 'attack1Monk',
            'attack2': 'attack2Monk',
            'attack3': 'attack3Monk',
            'specialAttack': 'specialAttackMonk',
            'meditate': 'meditateMonk',
            'roll': 'rollMonk',
            'defend': 'defendMonk',
            'takeHit': 'takeHitMonk',
            'death': 'deathMonk'
        };

    constructor(scene) {
      this.scene = scene;
      this.clazz = 'monk';
    }

    create() {
        let idleFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 6, zeroPad:0,
            prefix: 'idle/idle_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'idleMonk', frames: idleFrames, frameRate: 10, repeat: -1 });

        let runFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 8, zeroPad:0,
            prefix: 'run/run_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'runMonk', frames: runFrames, frameRate: 10, repeat: 0 });

        let jumpUpFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 3, zeroPad:0,
            prefix: 'j_up/j_up_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'jumpUpMonk', frames: jumpUpFrames, frameRate: 10, repeat: 0 });

        let jumpDownFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 3, zeroPad:0,
            prefix: 'j_down/j_down_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'jumpDownMonk', frames: jumpDownFrames, frameRate: 10, repeat: 0 });

        let airAttackFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 7, zeroPad:0,
            prefix: 'air_atk/air_atk_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'airAttackMonk', frames: airAttackFrames, frameRate: 10, repeat: 0 });

        let attack1Frames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 6, zeroPad:0,
            prefix: '1_atk/1_atk_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'attack1Monk', frames: attack1Frames, frameRate: 10, repeat: 0 });

        let attack2Frames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 12, zeroPad:0,
            prefix: '2_atk/2_atk_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'attack2Monk', frames: attack2Frames, frameRate: 10, repeat: 0 });

        let attack3Frames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 23, zeroPad:0,
            prefix: '3_atk/3_atk_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'attack3Monk', frames: attack3Frames, frameRate: 10, repeat: 0 });

        let specialAttackFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 25, zeroPad:0,
            prefix: 'sp_atk/sp_atk_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'specialAttackMonk', frames: specialAttackFrames, frameRate: 10, repeat: 0 });

        let meditateFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 16, zeroPad:0,
            prefix: 'meditate/meditate_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'meditateMonk', frames: meditateFrames, frameRate: 10, repeat: 0 });

        let rollFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 4, zeroPad:0,
            prefix: 'roll/roll_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'rollMonk', frames: rollFrames, frameRate: 10, repeat: 0 });

        let defendFrames  = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 13, zeroPad:0,
            prefix: 'defend/defend_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'defendMonk', frames: defendFrames, frameRate: 10, repeat: 0 });

        let takeHitFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 6, zeroPad:0,
            prefix: 'take_hit/take_hit_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'takeHitMonk', frames: takeHitFrames, frameRate: 10, repeat: 0 });

        let deathFrames = this.scene.anims.generateFrameNames('monk', {
            start: 1, end: 18, zeroPad:0,
            prefix: 'death/death_', suffix: '.png'
        });
        this.scene.anims.create({ key: 'deathMonk', frames: deathFrames, frameRate: 10, repeat: 0 });

        return this;
    }

    anim(key) {
        return this.#anims[key];
    }
}
