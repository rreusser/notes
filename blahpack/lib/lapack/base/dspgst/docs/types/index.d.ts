

// TypeScript declarations for @stdlib/lapack/base/dspgst

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduces a real symmetric-definite generalized eigenproblem to standard form using packed storage.
	*/
	(
		itype: number,
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		BP: Float64Array,
		strideBP: number,
		offsetBP: number
	): Float64Array;
}

/**
* Reduces a real symmetric-definite generalized eigenproblem to standard form using packed storage.
*/
declare var dspgst: Routine;

export = dspgst;
