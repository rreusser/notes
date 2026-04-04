

// TypeScript declarations for @stdlib/lapack/base/dtbrfs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Provides error bounds for the solution to a system with a real triangular band matrix.
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		kd: number,
		nrhs: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
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
* Provides error bounds for the solution to a system with a real triangular band matrix.
*/
declare var dtbrfs: Routine;

export = dtbrfs;
