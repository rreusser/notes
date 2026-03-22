

// TypeScript declarations for @stdlib/lapack/base/dlae2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute eigenvalues of a 2-by-2 symmetric matrix
	*/
	(
		a: number,
		b: number,
		c: number,
		rt1: number,
		rt2: number
	): void;
}

/**
* Compute eigenvalues of a 2-by-2 symmetric matrix
*/
declare var dlae2: Routine;

export = dlae2;
