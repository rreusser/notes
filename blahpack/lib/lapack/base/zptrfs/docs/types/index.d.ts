

// TypeScript declarations for @stdlib/lapack/base/zptrfs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Improves solution to a complex Hermitian tridiagonal system and provides error bounds
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		DF: Float64Array,
		strideDF: number,
		offsetDF: number,
		EF: Float64Array,
		strideEF: number,
		offsetEF: number,
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
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Improves solution to a complex Hermitian tridiagonal system and provides error bounds
*/
declare var zptrfs: Routine;

export = zptrfs;
