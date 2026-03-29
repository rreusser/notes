

// TypeScript declarations for @stdlib/lapack/base/zlaesy

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the eigenvalues and eigenvectors of a 2-by-2 complex symmetric matrix.
	*/
	(
		a: any,
		b: any,
		c: any,
		rt1: any,
		rt2: any,
		evscal: any,
		cs1: any,
		sn1: any
	): void;
}

/**
* Compute the eigenvalues and eigenvectors of a 2-by-2 complex symmetric matrix.
*/
declare var zlaesy: Routine;

export = zlaesy;
