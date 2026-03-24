

// TypeScript declarations for @stdlib/blas/base/drotg

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Construct a Givens plane rotation
	*/
	(
		a: number,
		b: number,
		c: number,
		s: number
	): void;
}

/**
* Construct a Givens plane rotation
*/
declare var drotg: Routine;

export = drotg;
