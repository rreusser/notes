

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgehd2 = require( './zgehd2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgehd2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgehd2;
