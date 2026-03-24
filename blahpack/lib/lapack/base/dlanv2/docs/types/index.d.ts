

// TypeScript declarations for @stdlib/lapack/base/dlanv2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the Schur factorization of a 2x2 nonsymmetric matrix
	*/
	(
		a: number,
		b: number,
		c: number,
		d: number,
		rt1r: number,
		rt1i: number,
		rt2r: number,
		rt2i: number,
		cs: number,
		sn: number
	): void;
}

/**
* Computes the Schur factorization of a 2x2 nonsymmetric matrix
*/
declare var dlanv2: Routine;

export = dlanv2;
