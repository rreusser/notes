

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zunmbr = require( './zunmbr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunmbr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunmbr;
