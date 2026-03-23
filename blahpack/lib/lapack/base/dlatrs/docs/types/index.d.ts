

// TypeScript declarations for @stdlib/lapack/base/dlatrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a triangular system with scaling to prevent overflow
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		normin: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		scale: number,
		CNORM: Float64Array,
		strideCNORM: number,
		offsetCNORM: number
	): Float64Array;
}

/**
* Solve a triangular system with scaling to prevent overflow
*/
declare var dlatrs: Routine;

export = dlatrs;
