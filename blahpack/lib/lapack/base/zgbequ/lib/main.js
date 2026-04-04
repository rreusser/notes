
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgbequ = require( './zgbequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbequ;
