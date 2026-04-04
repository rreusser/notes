

// TypeScript declarations for @stdlib/lapack/base/dspsvx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a real system A * X = B where A is symmetric in packed storage, with condition estimation and error bounds.
	*/
	(
		fact: string,
		uplo: string,
		N: number,
		nrhs: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		AFP: Float64Array,
		strideAFP: number,
		offsetAFP: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
		rcond: number,
		FERR: Float64Array,
		strideFERR: number,
		offsetFERR: number,
		BERR: Float64Array,
		strideBERR: number,
		offsetBERR: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number
	): Float64Array;
}

/**
* Solves a real system A * X = B where A is symmetric in packed storage, with condition estimation and error bounds.
*/
declare var dspsvx: Routine;

export = dspsvx;
