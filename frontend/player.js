export class Player {

  constructor(gameObj, clazz, ctrl) {
    this.gameObj = gameObj;
    this.clazz = clazz;
    this.ctrl = ctrl;
    this.health = 100;
    this.stamina = 100;
    this.state = 'idle';
    this.timeEnteredState = 0;
    this.animationDuration = 0;
  }

  update(time, delta) {
    switch(this.state) {
      case 'idle':
        if (this.ctrl.cursorsKeys.left.isDown) {
          this.state = 'run-left';
        } else if (this.ctrl.cursorsKeys.right.isDown) {
          this.state = 'run-right';
        } else if (this.ctrl.buttonC.isDown) {
          this.state = 'jump';
        }
        var idleAnim = this.clazz.anim('idle');
        this.gameObj.anims.play(idleAnim, true);
        this.gameObj.setVelocityX(0);
        break;
      case 'run-left':
        if (!this.ctrl.cursorsKeys.left.isDown) {
          this.state = 'idle';
        } else if (this.ctrl.buttonC.isDown) {
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
        } else if (this.ctrl.buttonC.isDown) {
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
            && this.ctrl.buttonC.isDown) {
          this.state = 'double-jump';
        } else if (this.gameObj.body.velocity.y >= 0) {
          this.state = 'jump-down-in-air';
        }
        var jumpAnim = this.clazz.anim('jumpUp');
        this.gameObj.anims.play(jumpAnim, true);
        break;
      case 'jump-down-in-air':
        if (this.canDoubleJump(time, this.timeEnteredState)
            && this.ctrl.buttonC.isDown) {
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
    }
  }

  canDoubleJump(now, timeEntered) {
    return now - timeEntered >= 500;
  }

}
