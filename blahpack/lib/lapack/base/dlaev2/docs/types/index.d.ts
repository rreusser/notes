

// TypeScript declarations for @stdlib/lapack/base/dlaev2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute eigendecomposition of a 2-by-2 symmetric matrix
	*/
	(
		a: number,
		b: number,
		c: number,
		rt1: number,
		rt2: number,
		cs1: number,
		sn1: number
	): void;
}

/**
* Compute eigendecomposition of a 2-by-2 symmetric matrix
*/
declare var dlaev2: Routine;

export = dlaev2;
