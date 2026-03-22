

// TypeScript declarations for @stdlib/lapack/base/dlas2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute singular values of a 2-by-2 triangular matrix
	*/
	(
		f: number,
		g: number,
		h: number,
		ssmin: number,
		ssmax: number
	): void;
}

/**
* Compute singular values of a 2-by-2 triangular matrix
*/
declare var dlas2: Routine;

export = dlas2;
