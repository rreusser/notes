

// TypeScript declarations for @stdlib/blas/base/zrotg

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Construct a Givens plane rotation with real cosine and complex sine.
	*/
	(
		a: number,
		b: number,
		c: number,
		s: number
	): void;
}

/**
* Construct a Givens plane rotation with real cosine and complex sine.
*/
declare var zrotg: Routine;

export = zrotg;
