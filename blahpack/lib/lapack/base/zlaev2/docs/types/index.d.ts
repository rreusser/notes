

// TypeScript declarations for @stdlib/lapack/base/zlaev2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the eigenvalues and eigenvectors of a 2-by-2 Hermitian matrix.
	*/
	(
		a: any,
		b: any,
		c: any,
		rt1: number,
		rt2: number,
		cs1: number,
		sn1: any
	): void;
}

/**
* Computes the eigenvalues and eigenvectors of a 2-by-2 Hermitian matrix.
*/
declare var zlaev2: Routine;

export = zlaev2;
