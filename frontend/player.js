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
        }

        break;
      case 'run-left':
        let runAnim = this.clazz.anim('run');
        this.gameObj.setFlipX(true);
        this.gameObj.setVelocityX(-160);
        this.gameObj.anims.play(runAnim, true);
        break;
      case 'run-right':
        break;
      case 'jump':
        break;
      case 'double-jump':
        break;
    }
  }

}


    /*
    if (cursorsKeys.left.isDown || leftKey.isDown) {
      player.setVelocityX(-160);
            playAnim(player, 'run');
            player.setFlipX(true);
        } else if (cursorsKeys.right.isDown || rightKey.isDown) {
            player.setVelocityX(160);
            playAnim(player, 'run');
            player.setFlipX(false);
        } else {
            player.setVelocityX(0);
        }

        if (buttonA.isDown) {
            playAnim(player, 'roll');
        }

        if (buttonB.isDown) {
            playAnim(player, 'attack1');
        }

        if (buttonC.isDown) {
            playAnim(player, 'airAttack');
        }

        if (buttonD.isDown) {
            playAnim(player, 'defend')
        }
        */
