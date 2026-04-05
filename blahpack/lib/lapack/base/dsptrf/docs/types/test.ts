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

import dsptrf = require( './index' );


// TESTS //

// The function returns a Float64Array...
{
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectType Float64Array
}

// The compiler throws an error if the function is provided a first argument which is not a string...
{
	dsptrf( 10, 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( true, 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( false, 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( null, 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( undefined, 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( [], 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( {}, 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a number...
{
	dsptrf( 'no-transpose', '10', new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', true, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', false, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', null, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', undefined, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', [], new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', {}, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a Float64Array...
{
	dsptrf( 'no-transpose', 10, '10', 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, 10, 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, true, 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, false, 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, null, 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, undefined, 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, [], 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, {}, 10, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a number...
{
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), '10', 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), true, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), false, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), null, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), undefined, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), [], 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), {}, 10, new Int32Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a number...
{
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, '10', new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, true, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, false, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, null, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, undefined, new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, [], new Int32Array( 25 ), 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, {}, new Int32Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a sixth argument which is not an Int32Array...
{
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, '10', 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, true, 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, false, 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, null, 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, undefined, 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, [], 10, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, {}, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a number...
{
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), '10', 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), true, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), false, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), null, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), undefined, 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), [], 10 ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), {}, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a eighth argument which is not a number...
{
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, '10' ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, true ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, false ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, null ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, undefined ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, [] ); // $ExpectError
	dsptrf( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Int32Array( 25 ), 10, {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	dsptrf(); // $ExpectError
	dsptrf( 'no-transpose' ); // $ExpectError
}
