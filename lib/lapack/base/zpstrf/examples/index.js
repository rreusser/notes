/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

var Complex128Array = require( '@stdlib/array/complex128' ); // eslint-disable-line stdlib/require-globals
var Float64Array = require( '@stdlib/array/float64' ); // eslint-disable-line stdlib/require-globals
var Int32Array = require( '@stdlib/array/int32' ); // eslint-disable-line stdlib/require-globals
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpstrf = require( './../lib' );

// 3x3 Hermitian positive semi-definite matrix A (column-major, upper storage):

// [10      (2+i)   (3-2i) ]

// [(2-i)    8      (1+i)  ]

// [(3+2i)  (1-i)    6     ]
var A = new Complex128Array( [ 10.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 1.0, 8.0, 0.0, 0.0, 0.0, 3.0, -2.0, 1.0, 1.0, 6.0, 0.0 ] ); // eslint-disable-line max-len
var PIV = new Int32Array( 3 );
var RANK = new Int32Array( 1 );
var WORK = new Float64Array( 6 );

var info = zpstrf( 'column-major', 'upper', 3, A, 3, PIV, RANK, -1.0, WORK );

console.log( 'info:', info );
console.log( 'A (factored):', reinterpret( A, 0 ) );
console.log( 'PIV:', PIV );
console.log( 'RANK:', RANK[ 0 ] );
