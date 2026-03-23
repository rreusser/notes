

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zhetrd = require( './zhetrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhetrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhetrd;
