export class LinalgError extends Error {
  constructor(routine, info) {
    super(`${routine}: parameter ${info} had an illegal value`);
    this.routine = routine;
    this.info = info;
    this.name = "LinalgError";
  }
}

export function xerbla(routine, info) {
  throw new LinalgError(routine.trim(), info);
}

export function lsame(a, b) {
  return a.charAt(0).toUpperCase() === b.charAt(0).toUpperCase();
}
