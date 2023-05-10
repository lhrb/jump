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
        } else if (this.ctrl.buttonF.isDown) {
          this.state = 'dodge';
        } else if (this.ctrl.buttonC.isDown) {
          this.state = 'air-attack';
        } else if (this.ctrl.buttonD.isDown) {
          this.state = 'special-attack';
        } else if (this.ctrl.buttonB.isDown) {
          this.state = 'defend';
        }
        this.playAnim('idle');
        this.gameObj.setVelocityX(0);
        // reset old state
        this.timeEnteredState = 0;
        this.animationDuration = 0;
        break;
      case 'run-left':
        if (!this.ctrl.cursorsKeys.left.isDown) {
          this.state = 'idle';
        } else if (this.ctrl.buttonE.isDown) {
          this.state = 'jump';
        }
        this.gameObj.setFlipX(true);
        this.gameObj.setVelocityX(-160);
        this.playAnim('run');
        break;
      case 'run-right':
        if (!this.ctrl.cursorsKeys.right.isDown) {
          this.state = 'idle';
        } else if (this.ctrl.buttonE.isDown) {
          this.state = 'jump';
        }
        this.gameObj.setFlipX(false);
        this.gameObj.setVelocityX(160);
        this.playAnim('run');
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
        this.playAnim('jumpDown');
        break;
      case 'double-jump':
        this.gameObj.setVelocityY(-200);
        this.state = 'double-jump-up-in-air';
        break;
      case 'double-jump-up-in-air':
        if (this.gameObj.body.velocity.y >= 0) {
          this.state = 'double-jump-down-in-air';
        }
        this.playAnim('jumpUp');
        break;
      case 'double-jump-down-in-air':
        if (this.gameObj.body.onFloor()) {
          this.state = 'idle';
        }
        this.playAnim('jumpDown');
        break;
      case 'charge':
        this.timeEnteredState += delta;
        while (this.timeEnteredState > 10) {
          this.timeEnteredState -= 10;
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
        this.playAnim('attack1');
        this.timeEnteredState = time;
        this.animationDuration = this.gameObj.anims.duration;
        this.state = 'attack1-anim';
        break;
      case 'attack1-anim':
        this.awaitAnim(time);
        break;
      case 'attack2':
        this.playAnim('attack2');
        this.timeEnteredState = time;
        this.animationDuration = this.gameObj.anims.duration;
        this.state = 'attack2-anim';
        break;
      case 'attack2-anim':
        this.awaitAnim(time);
        break;
      case 'attack3':
        this.playAnim('attack3');
        this.timeEnteredState = time;
        this.animationDuration = this.gameObj.anims.duration;
        this.state = 'attack3-anim';
        break;
      case 'attack3-anim':
        this.awaitAnim(time);
        break;
      case 'dodge':
        this.playAnim('roll');
        this.timeEnteredState = time;
        this.animationDuration = this.gameObj.anims.duration;
        this.state = 'dodge-anim';
        break;
      case 'dodge-anim':
        this.awaitAnim(time);
        if (this.gameObj.flipX) {
          this.gameObj.setVelocityX(-160);
        } else {
          this.gameObj.setVelocityX(160);
        }
        break;
      case 'air-attack':
        this.playAnim('airAttack');
        this.timeEnteredState = time;
        this.animationDuration = this.gameObj.anims.duration;
        this.state = 'attack3-anim';
        break;
      case 'air-attack-anim':
        this.awaitAnim(time);
        break;
      case 'special-attack':
        this.playAnim('specialAttack');
        this.timeEnteredState = time;
        this.animationDuration = this.gameObj.anims.duration;
        this.state = 'special-attack-anim';
        break;
      case 'special-attack-anim':
        this.awaitAnim(time);
        break;
      case 'defend':
        this.playAnim('defend');
        this.timeEnteredState = time;
        this.state = 'defend-hold';
        break;
      case 'defend-hold':
        this.playAnim('defend');
        if (!this.ctrl.buttonB.isDown) {
          this.state = 'idle';
        }
        break;
    }
  }

  /**
   * attack animation commitment
   */
  awaitAnim(time) {
    if (time - this.timeEnteredState >= this.animationDuration) {
          this.timeEnteredState = 0;
          this.animationDuration = 0;
          this.state = 'idle';
    }
  }

  canDoubleJump(now, timeEntered) {
    return now - timeEntered >= 500;
  }


  playAnim(anim) {
    var charAnim = this.clazz.anim(anim);
    this.gameObj.anims.play(charAnim, true);
  }

}
