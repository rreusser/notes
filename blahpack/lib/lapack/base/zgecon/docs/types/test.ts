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

import zgecon = require( './index' );


// TESTS //

// The function returns a Float64Array...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectType Float64Array
}

// The compiler throws an error if the function is provided a first argument which is not a string...
{
	zgecon( 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( true, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( false, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( null, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( undefined, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( [], 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( {}, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a second argument which is not a number...
{
	zgecon( 'no-transpose', '10', new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', true, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', false, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', null, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', undefined, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', [], new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', {}, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a third argument which is not a Float64Array...
{
	zgecon( 'no-transpose', 10, '10', 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, 10, 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, true, 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, false, 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, null, 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, undefined, 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, [], 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, {}, 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourth argument which is not a number...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), '10', 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), true, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), false, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), null, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), undefined, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), [], 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), {}, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fifth argument which is not a number...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, '10', 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, true, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, false, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, null, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, undefined, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, [], 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, {}, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a sixth argument which is not a number...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, '10', 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, true, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, false, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, null, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, undefined, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, [], 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, {}, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a seventh argument which is not a number...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, '10', 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, true, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, false, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, null, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, undefined, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, [], 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, {}, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a eighth argument which is not a number...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, '10', new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, true, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, false, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, null, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, undefined, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, [], new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, {}, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a ninth argument which is not a Float64Array...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, '10', 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, true, 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, false, 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, null, 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, undefined, 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, [], 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, {}, 10, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a tenth argument which is not a number...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), '10', 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), true, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), false, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), null, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), undefined, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), [], 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), {}, 10, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a eleventh argument which is not a number...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, '10', new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, true, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, false, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, null, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, undefined, new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, [], new Float64Array( 25 ), 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, {}, new Float64Array( 25 ), 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a twelfth argument which is not a Float64Array...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, '10', 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, 10, 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, true, 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, false, 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, null, 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, undefined, 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, [], 10, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, {}, 10, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a thirteenth argument which is not a number...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), '10', 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), true, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), false, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), null, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), undefined, 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), [], 10 ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), {}, 10 ); // $ExpectError
}

// The compiler throws an error if the function is provided a fourteenth argument which is not a number...
{
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, '10' ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, true ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, false ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, null ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, undefined ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, [] ); // $ExpectError
	zgecon( 'no-transpose', 10, new Float64Array( 25 ), 10, 10, 10, 10, 10, new Float64Array( 25 ), 10, 10, new Float64Array( 25 ), 10, {} ); // $ExpectError
}

// The compiler throws an error if the function is provided an unsupported number of arguments...
{
	zgecon(); // $ExpectError
	zgecon( 'no-transpose' ); // $ExpectError
}
