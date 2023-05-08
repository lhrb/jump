export class Player {

  constructor(gameObj, clazz, ctrl, bars) {
    this.gameObj = gameObj;
    this.clazz = clazz;
    this.ctrl = ctrl;
    this.health = 100;
    this.stamina = 100;
    this.power = 0;
    this.state = 'idle';
    this.timeEnteredState = 0;
    this.animationDuration = 0;
    this.bars = bars;
  }

  update(time, delta) {
    switch(this.state) {
      case 'idle':
        if (this.ctrl.cursorsKeys.left.isDown) {
          this.state = 'run-left';
        } else if (this.ctrl.cursorsKeys.right.isDown) {
          this.state = 'run-right';
        } else if (this.ctrl.buttonE.isDown) {
          this.state = 'jump';
        } else if (this.ctrl.buttonA.isDown) {
          this.state = 'charge';
        }
        var idleAnim = this.clazz.anim('idle');
        this.gameObj.anims.play(idleAnim, true);
        this.gameObj.setVelocityX(0);
        break;
      case 'run-left':
        if (!this.ctrl.cursorsKeys.left.isDown) {
          this.state = 'idle';
        } else if (this.ctrl.buttonE.isDown) {
          this.state = 'jump';
        }
        var runAnim = this.clazz.anim('run');
        this.gameObj.setFlipX(true);
        this.gameObj.setVelocityX(-160);
        this.gameObj.anims.play(runAnim, true);
        break;
      case 'run-right':
        if (!this.ctrl.cursorsKeys.right.isDown) {
          this.state = 'idle';
        } else if (this.ctrl.buttonE.isDown) {
          this.state = 'jump';
        }
        var runAnim = this.clazz.anim('run');
        this.gameObj.setFlipX(false);
        this.gameObj.setVelocityX(160);
        this.gameObj.anims.play(runAnim, true);
        break;
      case 'jump':
        this.gameObj.setVelocityY(-200);
        this.state = 'jump-up-in-air';
        this.timeEnteredState = time;
        break;
      case 'jump-up-in-air':
        if (this.canDoubleJump(time, this.timeEnteredState)
            && this.ctrl.buttonE.isDown) {
          this.state = 'double-jump';
        } else if (this.gameObj.body.velocity.y >= 0) {
          this.state = 'jump-down-in-air';
        }
        var jumpAnim = this.clazz.anim('jumpUp');
        this.gameObj.anims.play(jumpAnim, true);
        break;
      case 'jump-down-in-air':
        if (this.canDoubleJump(time, this.timeEnteredState)
            && this.ctrl.buttonE.isDown) {
          this.state = 'double-jump';
        } else if (this.gameObj.body.onFloor()) {
          this.state = 'idle';
        }
        var jumpAnim = this.clazz.anim('jumpDown');
        this.gameObj.anims.play(jumpAnim, true);
        break;
      case 'double-jump':
        this.gameObj.setVelocityY(-200);
        this.state = 'double-jump-up-in-air';
        break;
      case 'double-jump-up-in-air':
        if (this.gameObj.body.velocity.y >= 0) {
          this.state = 'double-jump-down-in-air';
        }
        var jumpAnim = this.clazz.anim('jumpUp');
        this.gameObj.anims.play(jumpAnim, true);
        break;
      case 'double-jump-down-in-air':
        if (this.gameObj.body.onFloor()) {
          this.state = 'idle';
        }
        var jumpAnim = this.clazz.anim('jumpDown');
        this.gameObj.anims.play(jumpAnim, true);
        break;
      case 'charge':
        this.timeEnteredState += delta;
        while (this.timeEnteredState > 25) {
          this.timeEnteredState -= 25;
          this.power += 1 % 100;
        }
        this.bars.powerBar(this.power / 100);
        if (!this.ctrl.buttonA.isDown) {
          if (this.power < 33) {
            this.state = 'attack1';
          } else if (this.power < 66) {
            this.state = 'attack2';
          } else {
            this.state = 'attack3';
          }
          this.power = 0;
          this.bars.powerBar(0);
        }
        break;
      case 'attack1':
        var atk1Anim = this.clazz.anim('attack1');
        this.gameObj.anims.play(atk1Anim, true);
        this.timeEnteredState = time;
        this.animationDuration = this.gameObj.anims.duration;
        this.state = 'attack1-anim';
        break;
      case 'attack1-anim':
        this.attackAnim(time);
        break;
      case 'attack2':
        var atk2Anim = this.clazz.anim('attack2');
        this.gameObj.anims.play(atk2Anim, true);
        this.timeEnteredState = time;
        this.animationDuration = this.gameObj.anims.duration;
        this.state = 'attack2-anim';
        break;
      case 'attack2-anim':
        this.attackAnim(time);
        break;
      case 'attack3':
        var atk3Anim = this.clazz.anim('attack3');
        this.gameObj.anims.play(atk3Anim, true);
        this.timeEnteredState = time;
        this.animationDuration = this.gameObj.anims.duration;
        this.state = 'attack3-anim';
        break;
      case 'attack3-anim':
        this.attackAnim(time);
        break;
    }
  }

  /**
   * attack animation commitment
   */
  attackAnim(time) {
    if (time - this.timeEnteredState >= this.animationDuration) {
          this.timeEnteredState = 0;
          this.animationDuration = 0;
          this.state = 'idle';
    }
  }

  canDoubleJump(now, timeEntered) {
    return now - timeEntered >= 500;
  }

}
