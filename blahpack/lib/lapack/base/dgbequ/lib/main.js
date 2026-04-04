
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgbequ = require( './dgbequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbequ;
