

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlaqp2 = require( './zlaqp2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaqp2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaqp2;
