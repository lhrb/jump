export class Player {

  constructor(clazz, ctrl) {
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
        break;
      case 'run-left':
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
