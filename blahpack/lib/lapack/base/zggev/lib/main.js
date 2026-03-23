

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zggev = require( './zggev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zggev, 'ndarray', ndarray );


// EXPORTS //

module.exports = zggev;
