import { PvsProcess } from "./server/pvsProcess";
import { PvsResponseType } from "./server/common/serverInterface";
import * as fsUtils from "./server/common/fsUtils";

const VERBOSE: boolean = false;
function log (data: any) {
	if (VERBOSE) {
		console.log("\n", data);
	}
}

describe("PvsProcess", async () => {
	log("PvsProcess");
	const pvsPath: string = "~/Work/pvs-snapshots/pvs-7.0-851/";

	it("constructor should be able to create a pvs process", async () => {
		const proc = new PvsProcess({ pvsPath });
		const success = await proc.pvs();
		expect(success).toBeTruthy();
		const versionInfo: PvsResponseType = await proc.pvsVersionInformation();
		expect(versionInfo).not.toBeNull();
		expect(versionInfo.res).not.toBeNull();
		log(versionInfo.res);
	});

	it("constructor should be able to set context", async () => {
		const pvsContextFolder: string = "~/Work/";
		const proc = new PvsProcess({ pvsPath, pvsContextFolder });
		await proc.pvs();
		const ctx: string = await proc.getContextFolder();
		log(`Current context: ${ctx}`);
		expect(ctx).toEqual(pvsContextFolder);
	});

	it("changeContext() should be able to change context", async () => {
		const proc = new PvsProcess({ pvsPath });
		await proc.pvs();
		const ctx: string = await proc.getContextFolder();
		log(`Current context: ${ctx}`);
		await proc.changeContext("..");
		const new_ctx: string = await proc.getContextFolder();
		log(`New context: ${new_ctx}`);
		expect(new_ctx).not.toEqual(ctx);
		const sup: string = `${ctx.split("/").slice(0, -2).join("/")}/`;
		expect(new_ctx).toEqual(sup);
	});
});
