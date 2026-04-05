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

import dppequ = require( './index' );


// TESTS //

// The function returns a Float64Array...
{
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectType Float64Array
}

// The compiler throws an error if the function is provided a first argument which is not a string...
{
	dppequ( 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( true, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( false, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( null, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( undefined, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( [], 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( {}, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a number...
{
	dppequ( 'no-transpose', '10', new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', true, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', false, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', null, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', undefined, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', [], new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', {}, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a Float64Array...
{
	dppequ( 'no-transpose', 10, '10', 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, true, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, false, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, null, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, undefined, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, [], 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, {}, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a number...
{
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), '10', 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), true, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), false, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), null, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), undefined, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), [], 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), {}, 10, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a number...
{
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, '10', new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, true, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, false, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, null, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, undefined, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, [], new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, {}, new Float64Array( 25 ), 10, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a sixth argument which is not a Float64Array...
{
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, '10', 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, true, 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, false, 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, null, 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, undefined, 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, [], 10, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, {}, 10, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a number...
{
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), '10', 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), true, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), false, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), null, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), undefined, 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), [], 10, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), {}, 10, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a eighth argument which is not a number...
{
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, '10', 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, true, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, false, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, null, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, undefined, 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, [], 10, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, {}, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a ninth argument which is not a number...
{
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, '10', 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, true, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, false, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, null, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, undefined, 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, [], 10 ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, {}, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a tenth argument which is not a number...
{
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, '10' ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, true ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, false ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, null ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, undefined ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, [] ); // $ExpectError
	dppequ( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10, 10, {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	dppequ(); // $ExpectError
	dppequ( 'no-transpose' ); // $ExpectError
}
