/*
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

import dsdot = require( './index' );


// TESTS //

// The function returns a Float32Array...
{
	dsdot( 10, 0, 10, 10, 10, 0, 10, 10, 10 ); // $ExpectType Float32Array
}

// The compiler throws an error if the function is provided a first argument which is not a number...
{
	dsdot( '10', 0, 10, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( true, 0, 10, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( false, 0, 10, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( null, 0, 10, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( undefined, 0, 10, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( [], 0, 10, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( {}, 0, 10, 10, 10, 0, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a number...
{
	dsdot( 10, 0, '10', 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, true, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, false, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, null, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, undefined, 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, [], 10, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, {}, 10, 10, 0, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a number...
{
	dsdot( 10, 0, 10, '10', 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, true, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, false, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, null, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, undefined, 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, [], 10, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, {}, 10, 0, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a number...
{
	dsdot( 10, 0, 10, 10, '10', 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, true, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, false, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, null, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, undefined, 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, [], 0, 10, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, {}, 0, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a number...
{
	dsdot( 10, 0, 10, 10, 10, 0, '10', 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, true, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, false, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, null, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, undefined, 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, [], 10, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, {}, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a eighth argument which is not a number...
{
	dsdot( 10, 0, 10, 10, 10, 0, 10, '10', 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, true, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, false, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, null, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, undefined, 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, [], 10 ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, {}, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a ninth argument which is not a number...
{
	dsdot( 10, 0, 10, 10, 10, 0, 10, 10, '10' ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, 10, true ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, 10, false ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, 10, null ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, 10, undefined ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, 10, [] ); // $ExpectError
	dsdot( 10, 0, 10, 10, 10, 0, 10, 10, {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	dsdot(); // $ExpectError
	dsdot( 10 ); // $ExpectError
}
