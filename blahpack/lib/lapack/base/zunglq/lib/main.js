

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zunglq = require( './zunglq.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunglq, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunglq;
